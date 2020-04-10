module FireLink.BackEnd.LivenessAnalyser (
    analyse, initialNextUseState, generateInterferenceGraph, InterferenceGraph (..)
) where

import           Control.Monad.State
import           Data.Graph
import qualified Data.Map.Strict                     as Map
import           Data.Maybe                          (catMaybes, fromJust,
                                                      isJust)
import qualified Data.Set                            as Set
import           FireLink.BackEnd.CodeGenerator
import           FireLink.BackEnd.FlowGraphGenerator
import           TACType

-- | A pair representing an unique point in a program. Its meaning is to hold (blockIds, instructionIndex)
type ProgramPoint = (Int, Int)

-- | A set of live variables at a given time, represented by their unique string name
type LiveVariables = Set.Set TACSymEntry

-- | A map that points a variable to the instruction it's next used in (or none)
type NextUseMap = Map.Map TACSymEntry (Maybe Int)

-- | An object containing a given point's next use information: live variables and next-use information
data NextUseInfo = NextUseInfo {
    nuiLiveVars :: !LiveVariables,
    nuiNextUse :: !NextUseMap
}
instance Show NextUseInfo where
    show NextUseInfo {nuiLiveVars=vs, nuiNextUse=us } =
        "LiveVars: " ++ show (Set.toList vs) ++ ", NextUse: " ++ show (Map.toList us)

-- | Our StateT that will progressively fetch next use info on our basic block
type NextUseState = StateT NextUseInfo IO

-- | A list of pairs of TAC instructions with their respective next use info.
-- | Basically, a basic block (no pun intended) with each instruction tagged with such info.
type BlockLiveness = [(TAC, NextUseInfo)]

-- | A map that matches variable names to integer representations,
-- | and a graph that matches such representations' mutual interference
type InterferenceGraph = (Map.Map TACSymEntry Int, Graph)

-- | Given a basic block, builds and returns a list of the string-representation
-- | of all the variables used in the program (including temporals)
getAllVariables :: BasicBlock -> [TACSymEntry]
getAllVariables = foldr getVariables []
    where
        getVariables :: TAC -> [TACSymEntry] -> [TACSymEntry]
        getVariables (ThreeAddressCode _ a b c) xs = xs ++ catTACSymEntries (catMaybes [a, b, c])

-- | Given a basic block, returns a set that marks all of its variables as live
initialLiveVariables :: BasicBlock -> LiveVariables
initialLiveVariables = Set.fromList . getAllVariables

-- | Given a basic block, returns a map that assigns to every one of its variables
-- | the value of Nothing, indicating that its next use is undefined
initialNextUseMap :: BasicBlock -> NextUseMap
initialNextUseMap block =
    let vars = getAllVariables block
        pairedVars = [(x, Nothing) | x <- vars]
    in  Map.fromList pairedVars

-- | Given a basic block, returns its initial next-use state:
-- |  * All variables have an undefined next use
-- |  * All variables are live
initialNextUseState :: BasicBlock -> NextUseInfo
initialNextUseState block = NextUseInfo {
    nuiLiveVars = initialLiveVariables block,
    nuiNextUse = initialNextUseMap block
}

-- | Given a basic block, performs a backwards data-flow analysis
-- | in order to tag each instruction with its liveness information
analyse :: BasicBlock -> NextUseState BlockLiveness
analyse block = do
    -- Number the instructions, then reverse them (due to backwards flow)
    let numberedBlock = reverse $ zip [1..] block

    -- For each instruction, get their live variables
    reversedAnalysedBlock <- mapM getLiveVariables numberedBlock

    -- Reverse the result (to get the original order)
    return $ reverse reversedAnalysedBlock

-- | Given a TAC instruction, and running as a state transformer,
-- | anotates the instruction with the current next-use info
-- | and prepares the state for the following instruction to be
-- | analysed
getLiveVariables :: NumberedTAC -> NextUseState (TAC, NextUseInfo)
getLiveVariables (i, tac@(ThreeAddressCode _ a b c)) = do
    nu@NextUseInfo { nuiLiveVars=liveVars, nuiNextUse=nextUse } <- get

    -- mark a as non-live and next use undefined
    (liveVars', nextUse')  <- case a of
        Just (Id v) -> return (Set.delete v liveVars, Map.insert v Nothing nextUse)
        _ -> return (liveVars, nextUse)

    -- mark b, c as live and with current next use
    (liveVars'', nextUse'') <- case b of
        Just (Id v) -> return (Set.insert v liveVars', Map.insert v (Just i) nextUse')
        _ -> return (liveVars', nextUse')
    (newLiveVars, newNextUse) <- case c of
        Just (Id v) -> return (Set.insert v liveVars'', Map.insert v (Just i) nextUse'')
        _ -> return (liveVars'', nextUse'')

    -- update state
    put NextUseInfo { nuiLiveVars = newLiveVars, nuiNextUse = newNextUse }
    return (tac, nu)

-- | Given a basic block, generates the interference graph
-- | by checking all of its instructions, one by one, as an undirected
-- | graph.
-- | (helpful resource: http://lambda.uta.edu/cse5317/spring03/notes/node40.html)
generateInterferenceGraph :: BasicBlock -> InterferenceGraph
generateInterferenceGraph block =
    let nodes = getAllVariables block
        numberedNodes = zip nodes [1..]
        numberMap = Map.fromList numberedNodes
        upperBound = length numberedNodes
        stringEdges = getInterferenceEdges block
        nodupStringEdges = Set.toList $ Set.fromList stringEdges
        -- the graph must be created from pairs of integers, so we find each edge name's respective integer
        -- if they were enumerated in the generation order
        edges = [(fromJust $ Map.lookup x numberMap, fromJust $ Map.lookup y numberMap) | (x, y) <- nodupStringEdges]
    in (numberMap, buildG (0, upperBound) edges)
    where
        getInterferenceEdges :: BasicBlock -> [(TACSymEntry, TACSymEntry)]
        getInterferenceEdges [] = []
        getInterferenceEdges (ThreeAddressCode _ a b c:xs) =
            let usedVarsList = catTACSymEntries $ catMaybes [a, b, c]
                currentEdges = [(i, j) | i <- usedVarsList, j <- usedVarsList, i /= j]
            in  currentEdges ++ getInterferenceEdges xs
