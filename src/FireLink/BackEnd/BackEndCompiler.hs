module FireLink.BackEnd.BackEndCompiler (
    backend
) where

import           Control.Monad.RWS                          (runRWST)
import           Control.Monad.State
import           Data.Graph                                 (Graph)
import           Data.List                                  (intercalate)
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

    let (initialRegisterAssignment, (newNumberedBlocks, _)) = initialStep flowGraph dictionary


    putStrLn "Before first step"
    printBasicBlocks numberedBlocks
    putStrLn "After first step"
    printBasicBlocks newNumberedBlocks
    -- mapM_ print livenessAnalysisResult
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
