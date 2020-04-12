{-|
Module      : FireLink.BackEnd.RegisterAllocationProcess
Description : Register allocation algorithm by graph coloration
Stability   : experimental

Attempts to allocate registers by doing chaitan algorithm as described here:
https://cs.gmu.edu/~white/CS640/p98-chaitin.pdf

[@initialStep@] process all codeblocks as we have infinite registers. Also, on function calls,
all live variables at that moment are going to be "stored" before them. That is done to
avoid that the interference graph will have edges between their own variables.

Register allocation is made using optimistic colouring algorithm from Chaitan/Briggs
-}
module FireLink.BackEnd.RegisterAllocationProcess (initialStep, SymEntryRegisterMap, Register) where

import qualified Control.Monad.State                 as State
import qualified Data.Array                          as A
import           Data.Char                           (isDigit)
import qualified Data.Graph                          as G
import qualified Data.Map                            as Map
import qualified Data.Set                            as Set
import           Debug.Trace                         (trace)
import           FireLink.BackEnd.CodeGenerator      (TAC (..),
                                                      TACSymEntry (..))
import           FireLink.BackEnd.FlowGraphGenerator (BasicBlock,
                                                      FlowGraph (..),
                                                      NumberedBlock,
                                                      getProgramVariables)
import           FireLink.BackEnd.LivenessAnalyser
import           FireLink.BackEnd.Utils
import           FireLink.FrontEnd.SymTable          (Dictionary (..),
                                                      findArgsByFunName,
                                                      getOffset)
import qualified FireLink.FrontEnd.SymTable          as ST (DictionaryEntry (..))
import           TACType

-- Semantic aliases
type Register = Int
type Color = Register

-- | Associate a variable with a color (register). If the variable isn't in the
-- | map we can say that it needs to be spilled
type SymEntryRegisterMap = Map.Map TACSymEntry Color


-- | Coloration state for the optimistic coloring algorithm by Chaitan/Briggs, as exposed
-- | on classroom slides.
data ColorationState = ColorationState
    { csGraph :: !G.Graph -- ^ state graph
    , csDeletedVertices :: !(Set.Set G.Vertex) -- ^ Set of deleted vertices
    , csRegisterStack :: ![G.Vertex] -- ^ register stack
    , csProgramFlowGraph :: !FlowGraph -- ^ Whole program flow-graph = (NumberedBlocks, Graph)
    , csInterferenceGraph :: !InterferenceGraph -- ^ Current interference graph
    , csLivenessInformation :: ![LineLiveVariables] -- ^ in/out liveness information for each line
    }

-- | Data.Graph graph representation is made by a range of integers describeing the set of vertices
-- | So we can't exactly delete a single vertex, only remove all edges where it appears.
-- | Even Data.Graph.outdegree will say that its outdegree is 0, so we need to have a set of deleted vertices
-- | as well.
type ColorState = State.State ColorationState

-------------------------------------------------------
----- Helper functions to operate with ColorState -----
-------------------------------------------------------

-- | Helper to get whole program from state
getProgramFlowGraph :: ColorState FlowGraph
getProgramFlowGraph = csProgramFlowGraph <$> State.get

-- | Helper to replace interference graph on current state
putInterferenceGraph :: InterferenceGraph -> ColorState ()
putInterferenceGraph interferenceGraph = do
    c <- State.get
    State.put c{csInterferenceGraph = interferenceGraph}

-- | Helper to get interference grpah on current state
getInterferenceGraph :: ColorState InterferenceGraph
getInterferenceGraph = csInterferenceGraph <$> State.get

-- | helper to replace livenesss information
putLivenessInformation :: [LineLiveVariables] -> ColorState ()
putLivenessInformation ls = do
    c <- State.get
    State.put c{csLivenessInformation = ls}

-- | Helper to get the graph in a ColorState execution
getGraph :: ColorState G.Graph
getGraph = csGraph <$> State.get

putGraph :: G.Graph -> ColorState ()
putGraph graph = do
    c <- State.get
    State.put c{csGraph = graph}

-- | Helper to get deleted vertices set in a ColorState execution
getDeletedVertices :: ColorState (Set.Set G.Vertex)
getDeletedVertices = csDeletedVertices <$> State.get

putDeletedVertices :: Set.Set G.Vertex -> ColorState ()
putDeletedVertices vertices = do
    c <- State.get
    State.put c{csDeletedVertices = vertices}

-- | Helper to get current stack
getRegisterStack :: ColorState [G.Vertex]
getRegisterStack = csRegisterStack <$> State.get

-- | Helper to put register stack
putRegisterStack :: [G.Vertex] -> ColorState ()
putRegisterStack stack = do
    c <- State.get
    State.put c{csRegisterStack = stack}

-- | Push a register on top of the state's stack
pushRegister :: Register -> ColorState ()
pushRegister reg = do
    stack <- getRegisterStack
    putRegisterStack $ reg : stack


graphBounds :: G.Graph -> G.Bounds
graphBounds graph = (0, length $ G.vertices graph)

deleteVertex :: G.Vertex -> ColorState ()
deleteVertex vertex = do
    graph <- getGraph
    deletedVertices <- getDeletedVertices
    let newEdges = filter (\(a, b) -> a /= vertex && b /= vertex) $ G.edges graph

    putGraph $ G.buildG (graphBounds graph) newEdges
    putDeletedVertices $ vertex `Set.insert` deletedVertices


-- | Generate attempting final TAC assumming that we will have infinite registers i.e one register per actual variable
-- | Also, before and after functions call live variables at that point will be saved to and loaded from memory respectively.
-- | This is done to reduce the size of the interference graph, and because variables from different functions are not supposed
-- | to interfere between them.
initialStep :: FlowGraph -> Dictionary -> (SymEntryRegisterMap, FlowGraph)
initialStep flowGraph@(numberedBlocks, graph) dict =
    (variableRegisterMap, (preProcessCode, graph))
    where
        programVariables :: Set.Set TACSymEntry
        programVariables = getProgramVariables numberedBlocks

        -- | one for each actual variable
        initialRegisters :: [Register]
        initialRegisters = [0 .. Set.size programVariables - 1]

        -- | Map for each tac to its initial register
        variableRegisterMap :: SymEntryRegisterMap
        variableRegisterMap = Map.fromList $ zip (Set.toList programVariables) initialRegisters

        initialLivenessAnalysis :: [LineLiveVariables]
        initialLivenessAnalysis = livenessAnalyser flowGraph

        getLivenessIn :: ProgramPoint -> LineLiveVariables
        getLivenessIn point = head $ filter ((point ==) . llvInstrId) initialLivenessAnalysis

        isTacLabelFun :: TAC -> Bool
        isTacLabelFun (ThreeAddressCode NewLabel _ (Just (Label (l : _))) _) =
            -- case for _main function, that isn't on symtable
            l /= '_' &&
            -- jump labels, functions do not begin with digits
            not (isDigit l)

        isTacLabelFun _ = False

        preProcessCode :: [NumberedBlock]
        preProcessCode = map go numberedBlocks

        go :: NumberedBlock -> NumberedBlock
        go (index, block) =
            (index, concatMap (go' . \(instrIdx, tac) -> ((index, instrIdx), tac)) $ zip [0..] block)

        getFunctionArguments :: String -> [TACSymEntry]
        getFunctionArguments funName =
            let dictEntryArguments = findArgsByFunName funName dict in
                map (\entry -> TACVariable entry (getOffset entry)) dictEntryArguments

        go' :: (ProgramPoint, TAC) -> [TAC]
        -- If we find a function call, we need to store all live-variables at that point
        go' (programPoint, tac@(ThreeAddressCode Call maybeVar _ _)) =
            let liveVariablesInOut = getLivenessIn programPoint
                storeTacs = map (\tacSymEntry ->
                    ThreeAddressCode Store (Just (Id tacSymEntry)) Nothing Nothing) $
                        Set.toList $ llvInLiveVariables liveVariablesInOut
                toDeleteAfterCall = case maybeVar of
                                        Nothing     -> Set.empty
                                        Just (Id v) -> Set.singleton v
                loadTacs = map (\tacSymEntry ->
                    ThreeAddressCode Load (Just (Id tacSymEntry)) Nothing Nothing) $
                        Set.toList $ llvOutLiveVariables liveVariablesInOut Set.\\ toDeleteAfterCall
                in storeTacs ++ [tac] ++ loadTacs

        -- after generating code for labels, we need to load the used-arguments to their registers.
        -- if a var is not in the variableRegisterMap then it is not used in the program i.e. we can
        -- skip its load
        go' (_, tac@(ThreeAddressCode NewLabel _ (Just (Label funName)) _)) =
            if isTacLabelFun tac then
                let args = getFunctionArguments funName
                    loadTacsSet = map (\tacSymEntry ->
                        case variableRegisterMap Map.!? tacSymEntry of
                            Nothing -> []
                            Just _ -> [ThreeAddressCode Load (Just (Id tacSymEntry)) Nothing Nothing]) args
                    loadTacs = concat loadTacsSet in
                        tac : loadTacs
            else [tac]
        go' (_, tac) = [tac]


-- | Attempts to color a graph with the number of general-purpose registers
simplify :: ColorState ()
simplify = go
    where
        numberOfVertices :: ColorState Int
        numberOfVertices = do
            (_, interferenceGraph) <- getInterferenceGraph
            return $ length $ G.vertices interferenceGraph

        numberOfColors :: Int
        numberOfColors = length availableRegisters

        -- Attempt to choose a vertex
        chooseVertexWithLessthanK :: ColorState (Maybe G.Vertex)
        chooseVertexWithLessthanK = do
            graph <- getGraph
            deletedVertices <- getDeletedVertices
            let vertices = G.vertices graph
            let graphOutDegreePerVertex = A.assocs $ G.outdegree graph
            -- We can choose a vertex which outdegree is between 0 and "numberOfColors", inclusive and exclusive.
            -- Also, since Data.Graph doesn't provide the deletion of vertices, we need to keep a set of _deleted_
            -- vertices
            case filter (\(vertex, outdegree) ->
                    0 <= outdegree && outdegree < numberOfColors &&
                    vertex `Set.notMember` deletedVertices) graphOutDegreePerVertex of
                []         -> return Nothing
                (a, _) : _ -> return $ Just a


        -- Returns a stack of vertices that emulated the reverse order of vertices that were delete
        go :: ColorState ()
        go = do
            deletedVertices <- getDeletedVertices
            numOfVertices <- numberOfVertices
            -- | If every vertex was deleted, then coloration is finished
            if Set.size deletedVertices == numOfVertices then
                return ()
            -- | Otherwise, let's keep coloration
            else do
                maybeChoosenVertex <- chooseVertexWithLessthanK
                case maybeChoosenVertex of
                    -- | Nice! we can retrieve this vertex and add it to the stack
                    Just v -> do
                        deleteVertex v
                        pushRegister v
                        go
                    -- | TODO: handle case when we can't get a vertex with less than k neighbours
                    Nothing -> undefined

-- | Do liveness analysis on a program to construct the interference graph.
build :: ColorState ()
build = do
    flowGraph <- getProgramFlowGraph
    let (interferenceGraph, livenessInfo) = generateInterferenceGraph' flowGraph
    putInterferenceGraph interferenceGraph
    putLivenessInformation livenessInfo


-- | Actually runs chaitain/briggs optimistic coloring algorithm to produce new code with
-- | mappings between variables and their assigned registers
run :: FlowGraph -> (SymEntryRegisterMap, FlowGraph)
run flowGraph = State.evalState go (initialState flowGraph)
    where
        initialState :: FlowGraph -> ColorationState
        initialState flowGraph =
            ColorationState
                { csGraph = G.buildG (0, 1) [] -- dummy value
                , csDeletedVertices = Set.empty -- dummy value
                , csRegisterStack = [] -- dummy value
                , csProgramFlowGraph = flowGraph
                , csInterferenceGraph = (Map.empty, G.buildG (0, 1) []) -- dummy value
                , csLivenessInformation = [] -- dummy value
                }

        go :: ColorState (SymEntryRegisterMap, FlowGraph)
        go = do
            build
            -- TODO: implement `coalesce`, we can live without it by the moment
            -- costs

            simplify
            undefined

