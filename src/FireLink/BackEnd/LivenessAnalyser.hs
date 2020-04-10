module FireLink.BackEnd.LivenessAnalyser (
    generateInterferenceGraph, InterferenceGraph (..)
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
