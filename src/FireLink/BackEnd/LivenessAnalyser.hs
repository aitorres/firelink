module FireLink.BackEnd.LivenessAnalyser (
    def, generateInterferenceGraph, InterferenceGraph (..)
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
def1 :: TAC -> Set.Set TACSymEntry
def1 (ThreeAddressCode op (Just (Id v)) _ _) s
    | op `elem` assignableOperations = Set.singleton v
    | otherwise = Set.empty

-- | Read will end setting a value to its parameter
def1 (ThreeAddressCode Read _ (Just (Id v)) _) = Set.singleton v
def1 _ s = Set.empty

-- | Calculate used variables in a basic block prior to any definition of the same variable inside the
-- | same block
use :: BasicBlock -> Set.Set TACSymEntry
use = foldr use' Set.empty
    where
        use' :: TAC -> Set.Set TACSymEntry -> Set.Set TACSymEntry
        use' tac set = error ""

-- | Calculate used variables in a single instruction. That is, their operands
-- | TODO: review operations on the `otherwise` branch to see if the used values are actually ok
use1 :: TAC -> Set.Set TACSymEntry
use1 (ThreeAddressCode op u v w)
    | op `elem` assignableOperations = Set.fromList $ catTACSymEntries $ catMaybes [v, w]
    | otherwise = Set.fromList $ catTACSymEntries $ catMaybes [u, v, w]

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
