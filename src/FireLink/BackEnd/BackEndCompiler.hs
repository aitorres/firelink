module FireLink.BackEnd.BackEndCompiler (
    backend
) where

import           Control.Monad.RWS                          (runRWST)
import           Control.Monad.State
import           Data.Graph                                 (Graph)
import qualified Data.Graph                                 as G
import           Data.List                                  (intercalate)
import qualified Data.Map                                   as Map
import           FireLink.BackEnd.CodeGenerator             (CodeGenState (..),
                                                             SimpleType (..),
                                                             TAC (..),
                                                             TACSymEntry (..),
                                                             genCode,
                                                             initialState)
import           FireLink.BackEnd.FlowGraphGenerator        (NumberedBlock,
                                                             generateFlowGraph)
import           FireLink.BackEnd.InstructionCodeGenerator
import           FireLink.BackEnd.LivenessAnalyser
import           FireLink.BackEnd.Optimizer                 (optimize)
import           FireLink.BackEnd.RegisterAllocationProcess
import           FireLink.FrontEnd.Grammar                  (Program (..))
import           FireLink.FrontEnd.SymTable                 (Dictionary (..))
import           FireLink.Utils
import           TACType

backend :: Program -> Dictionary -> IO ([TAC], [(NumberedBlock, InterferenceGraph)], Graph, InterferenceGraph)
backend program dictionary = do
    (_, finalState, code) <- runRWST (genCode program) dictionary initialState
    let postProcessedCode = fillEmptyTemporals (cgsTemporalsToReplace finalState) code
    let optimizedCode = optimize postProcessedCode
    let flowGraph@(numberedBlocks, graph) = generateFlowGraph optimizedCode
    let basicBlocks = map snd numberedBlocks
    let interferenceGraphs = map generateInterferenceGraph basicBlocks
    let blocksWithInterGraphs = zip numberedBlocks interferenceGraphs
    let livenessAnalysisResult = livenessAnalyser flowGraph
    let interferenceGraph' = generateInterferenceGraph' flowGraph

    let (initialRegisterAssignment, flowGraph'@(newNumberedBlocks, _)) = initialStep flowGraph dictionary
    let interferenceGraph'' = generateInterferenceGraph' flowGraph'


    putStrLn "Before first step"
    printBasicBlocks numberedBlocks
    putStrLn "After first step"
    printBasicBlocks newNumberedBlocks
    mapM_ print livenessAnalysisResult

    putStrLn "Interference grahp before code manipulation assignment"
    printInterferenceGraph' interferenceGraph'

    putStrLn "Initial register assignment"
    printRegisterAssignment initialRegisterAssignment
    putStrLn "Interference grahp after codemanipulation assignment"
    printInterferenceGraph' interferenceGraph''
    return (optimizedCode, blocksWithInterGraphs, graph, interferenceGraph')
    where
        fillEmptyTemporals :: [(TACSymEntry, Int)] -> [TAC] -> [TAC]
        fillEmptyTemporals tempsToReplace = map (patchTac tempsToReplace)

        patchTac :: [(TACSymEntry, Int)] -> TAC -> TAC
        patchTac tempsToReplace tac = case tac of
            ThreeAddressCode Assign (Just (Id x)) (Just (Constant ("TO_REPLACE", _))) _ ->
                let l = filter (matchTemps x . fst) tempsToReplace in
                    if null l then tac
                    else let t = head l in
                        ThreeAddressCode
                            Assign
                            (Just (Id $ fst t))
                            (Just $ Constant (show $ snd t, BigIntTAC))
                            Nothing
            _ -> tac

        matchTemps :: TACSymEntry -> TACSymEntry -> Bool
        matchTemps (TACTemporal i _) (TACTemporal i' _) = i == i'
        matchTemps _ _                                  = False


printTacCode :: [TAC] -> IO ()
printTacCode = mapM_ print

printBasicBlocks :: [NumberedBlock] -> IO ()
printBasicBlocks = mapM_ printBlock
    where
        printBlock :: NumberedBlock -> IO ()
        printBlock nb = do
            let (i, cb) = nb
            putStrLn $ bold ++ "Block " ++ show i ++ nocolor
            printTacCode cb

printInterferenceGraph' :: InterferenceGraph -> IO ()
printInterferenceGraph' (vertexMap, graph) = do
    let vs = G.vertices graph
    let es = G.edges graph
    putStrLn $ bold ++ "Graph (" ++ (show . length) vs ++ " variables, " ++ (show . length) es ++ " interferences)" ++ nocolor
    mapM_ printEdges vs
    where
        successors :: G.Vertex -> [G.Vertex]
        successors vertex =
            let graphEdges = G.edges graph
                outgoingEdges = filter ((== vertex) . fst) graphEdges
                successors' = map snd outgoingEdges
            in successors'

        printEdges :: G.Vertex -> IO ()
        printEdges v = do
            let originName = show $ vertexMap Map.! v
            let destinies = successors v
            putStrLn $ bold ++ originName ++ nocolor ++ " -> " ++ printDestinies destinies

        printDestinies :: [Int] -> String
        printDestinies destinies = joinWithCommas $ map (vertexMap Map.!) destinies

printRegisterAssignment :: SymEntryRegisterMap -> IO ()
printRegisterAssignment registerMap = do
    let pairList = Map.toList registerMap
    mapM_ prettyPrint pairList
    where
        prettyPrint :: (TACSymEntry, Register) -> IO ()
        prettyPrint (tacSymEntry, register) =
            putStrLn $ bold ++ show tacSymEntry ++ nocolor ++ " => " ++ show register
