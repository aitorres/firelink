module FireLink.BackEnd.BackEndCompiler (
    backend
) where

import           Control.Monad.RWS                         (runRWST)
import           Data.Graph                                (Graph)
import           Data.List                                 (intercalate)
import           FireLink.BackEnd.CodeGenerator            (CodeGenState (..),
                                                            TAC (..),
                                                            TACSymEntry (..),
                                                            genCode,
                                                            initialState)
import           FireLink.BackEnd.FlowGraphGenerator       (NumberedBlocks,
                                                            findBasicBlocks,
                                                            generateFlowGraph,
                                                            numberTACs)
import           FireLink.BackEnd.InstructionCodeGenerator ()
import           FireLink.BackEnd.NaiveRegisterAllocator   (allocateRegisters, initialDescriptorLists)
import           FireLink.BackEnd.Optimizer                (optimize)
import           FireLink.FrontEnd.Grammar                 (Program (..))
import           FireLink.FrontEnd.SymTable                (Dictionary (..))
import           FireLink.FrontEnd.TypeChecking            (Type (..))
import           TACType

backend :: Program -> Dictionary -> IO ([TAC], NumberedBlocks, Graph, String)
backend program dictionary = do
    (_, finalState, code) <- runRWST (genCode program) dictionary initialState
    let postProcessedCode = fillEmptyTemporals (cgsTemporalsToReplace finalState) code
    let optimizedCode = optimize postProcessedCode
    let basicBlocks = zip [0..] $ findBasicBlocks $ numberTACs optimizedCode
    let flowGraph = generateFlowGraph optimizedCode

    -- ! TEMP
    allocatedTac <- mapM mockAllocate $ findBasicBlocks $ numberTACs optimizedCode
    -- ! TEMP

    return (optimizedCode, basicBlocks, flowGraph, intercalate "\n" allocatedTac)
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
                            (Just $ Constant (show $ snd t, BigIntT))
                            Nothing
            _ -> tac

        matchTemps :: TACSymEntry -> TACSymEntry -> Bool
        matchTemps (TACTemporal i _) (TACTemporal i' _) = i == i'
        matchTemps _ _                                  = False

-- ! TEMP
mockAllocate :: [TAC] ->  IO String
mockAllocate t = do
    (_, _, c) <- runRWST allocateRegisters t initialDescriptorLists
    return $ intercalate "\n" c
