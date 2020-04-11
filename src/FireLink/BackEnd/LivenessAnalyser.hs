module FireLink.BackEnd.LivenessAnalyser (
    def, use, generateInterferenceGraph, InterferenceGraph (..), livenessAnalyser
) where

import           Control.Monad.State
import qualified Data.Graph                          as Graph
import           Data.List                           (intercalate)
import qualified Data.Map.Strict                     as Map
import           Data.Maybe                          (catMaybes, fromJust,
                                                      isJust)
import qualified Data.Set                            as Set
import           Debug.Trace                         (trace)
import           FireLink.BackEnd.CodeGenerator
import           FireLink.BackEnd.FlowGraphGenerator
import           FireLink.BackEnd.Utils
import           TACType

-- | A map that matches variable names to integer representations,
-- | and a graph that matches such representations' mutual interference
type InterferenceGraph = (Map.Map TACSymEntry Int, Graph.Graph)

-- | Given a basic block, builds and returns a list of the string-representation
-- | of all the variables used in the program (including temporals)
getAllVariables :: BasicBlock -> [TACSymEntry]
getAllVariables = foldr getVariables []
    where
        getVariables :: TAC -> [TACSymEntry] -> [TACSymEntry]
        getVariables (ThreeAddressCode _ a b c) xs = xs ++ catTACSymEntries (catMaybes [a, b, c])

-- | Operations that consists of an assignment of a value to a lvalue
-- | TODO: Add pointer operations here when their implementation is ready
assignableOperations :: [Operation]
assignableOperations = [Assign, Add, Minus, Sub, Mult, Div, Mod, Get, Call, Set]

-- | Calculate variable definitions of a basic block, used by data-flow analysis algorithm for liveness analysis
-- | Mathematically, def[B] = union of def[n] for n in B.indices
def :: BasicBlock -> Set.Set TACSymEntry
def = foldr def' Set.empty
    where
        def' :: TAC -> Set.Set TACSymEntry -> Set.Set TACSymEntry
        def' tac s = s `Set.union` def1 tac

-- | Calculates definition for a single three-address code instruction. Basically, the left side of an instruction
-- | that assigns a variable.
def1 :: TAC -> Set.Set TACSymEntry
def1 (ThreeAddressCode op (Just (Id v)) _ _)
    | op `elem` assignableOperations = Set.singleton v

-- | Read will end setting a value to its parameter
def1 (ThreeAddressCode Read _ (Just (Id v)) _) = Set.singleton v
-- | Casting also assigns a value
def1 t@(ThreeAddressCode (Cast _ _) (Just (Id v)) _ _) = Set.singleton v
def1 _ = Set.empty

-- | Calculate used variables in a basic block prior to any definition of the same variable inside the
-- | same block. its mathematical definition is as follows:
-- | use[B] = use[1] U (use[2] - def[1]) U (use[3] - def[2] - def[1]) U ...
use :: BasicBlock -> Set.Set TACSymEntry
use = go [Set.empty]
    where
        -- | accumulatedDefs has the current differences from the useB mathematical definition
        go :: [Set.Set TACSymEntry] -> BasicBlock -> Set.Set TACSymEntry
        go accumulatedDefs (i : is) = diffOfList (use1 i : accumulatedDefs) `Set.union` go (def1 i : accumulatedDefs) is
        go _ []                     = Set.empty


        diffOfList :: Ord a => [Set.Set a] -> Set.Set a
        diffOfList (s : ss) = foldl Set.difference s ss

-- | Calculate used variables in a single instruction. That is, their operands
-- | TODO: review operations on the `otherwise` branch to see if the used values are actually ok
use1 :: TAC -> Set.Set TACSymEntry
use1 (ThreeAddressCode op u v w)
    | op `elem` assignableOperations = Set.fromList $ catTACSymEntries $ catMaybes [v, w]
    | isCast op = Set.fromList $ catTACSymEntries $ catMaybes [v, w]
    | otherwise = Set.fromList $ catTACSymEntries $ catMaybes [u, v, w]
    where
        isCast :: Operation -> Bool
        isCast (Cast _ _) = True
        isCast _          = False

-- | Semantic alias for Set.Set TACSymEntry
type LiveVariables = Set.Set TACSymEntry

data BlockLiveVariables = BlockLiveVariables
    { blvBlockId :: !Int -- ^ Block id
    , blvInLiveVariables :: !LiveVariables -- ^ live variables upon execution this block
    , blvOutLiveVariables :: !LiveVariables -- ^ live variables after execution this block
    }

instance Show BlockLiveVariables where
    show BlockLiveVariables
        { blvBlockId = blockId
        , blvInLiveVariables = blockIn
        , blvOutLiveVariables = blockOut } = "Block #" ++ show blockId ++ " in = " ++ intercalate ", " (map show (Set.toList blockIn)) ++ " out = " ++ intercalate ", " (map show (Set.toList blockOut))

-- | Semantic alias for (LivenessIn, LivenessOut)
type LivenessInOut = (LiveVariables, LiveVariables)

type DataFlowInfo = Map.Map Graph.Vertex LivenessInOut

-- | Calculates live variables at end
livenessAnalyser :: FlowGraph -> [BlockLiveVariables]
livenessAnalyser fg@(numberedBlocks, flowGraph) =
    -- | Number of actual blocks + exit + entry
    let livenessInOutMap = Map.fromList $ zip graphNodes $ repeat (Set.empty, Set.empty)
        convergedInOut = fixedPoint livenessAnalyser' livenessInOutMap
        in map (\(blockId, (blockIn, blockOut)) ->
                BlockLiveVariables
                    { blvBlockId = blockId
                    , blvInLiveVariables = blockIn
                    , blvOutLiveVariables = blockOut}) $ Map.toList convergedInOut

    where
        f :: (BasicBlock -> Set.Set TACSymEntry) -> Graph.Vertex -> Set.Set TACSymEntry
        f fun vertex
            | vertex == (-1) || vertex == length numberedBlocks = Set.empty
            | otherwise = fun $ snd $ head $ filter ((vertex ==) . fst) numberedBlocks

        graphNodes :: [Graph.Vertex]
        graphNodes = entryVertex fg : exit : map fst numberedBlocks

        useMap :: Map.Map Graph.Vertex LiveVariables
        useMap = Map.fromList $ zip graphNodes $ map (f use) graphNodes

        defMap :: Map.Map Graph.Vertex LiveVariables
        defMap = Map.fromList $ zip graphNodes $ map (f def) graphNodes

        useB :: Graph.Vertex -> LiveVariables
        useB = (useMap Map.!)

        defB :: Graph.Vertex -> LiveVariables
        defB = (defMap Map.!)

        successorsMap :: Map.Map Graph.Vertex (Set.Set Graph.Vertex)
        successorsMap = Map.fromList $ zip graphNodes $ map successors' graphNodes

        successors :: Graph.Vertex -> Set.Set Graph.Vertex
        successors = (successorsMap Map.!)

        successors' :: Graph.Vertex -> Set.Set Graph.Vertex
        successors' vertex =
            let graphEdges = Graph.edges flowGraph
                outgoingEdges = filter ((== vertex) . fst) graphEdges
                successors' = map snd outgoingEdges
                in Set.fromList successors'

        exit :: Graph.Vertex
        exit = exitVertex fg

        -- | First tuple corresponds too livenessIn, second one to livenessOut
        livenessAnalyser' :: DataFlowInfo -> DataFlowInfo
        livenessAnalyser' livenessInOutZipped = go livenessInOutZipped graphNodes
            where
                go :: DataFlowInfo -> [Graph.Vertex] -> DataFlowInfo
                go dfi [] = dfi
                go dfi (blockId : blockIds) =
                    if blockId == exit then go dfi blockIds
                    else
                        let outB = Set.unions $ Set.map (fst . (dfi Map.!)) $ successors blockId
                            inB = useB blockId `Set.union` (outB Set.\\ defB blockId)
                        in go (Map.insert blockId (inB, outB) dfi) blockIds

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
    in (numberMap, Graph.buildG (0, upperBound) edges)
    where
        getInterferenceEdges :: BasicBlock -> [(TACSymEntry, TACSymEntry)]
        getInterferenceEdges [] = []
        getInterferenceEdges (ThreeAddressCode _ a b c:xs) =
            let usedVarsList = catTACSymEntries $ catMaybes [a, b, c]
                currentEdges = [(i, j) | i <- usedVarsList, j <- usedVarsList, i /= j]
            in  currentEdges ++ getInterferenceEdges xs
