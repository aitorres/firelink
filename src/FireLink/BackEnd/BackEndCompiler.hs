module FireLink.BackEnd.BackEndCompiler (
    backend
) where

import           Control.Monad.RWS                         (runRWST)
import           Data.Graph                                (Graph)
import           Data.List                                 (intercalate)
import           FireLink.BackEnd.CodeGenerator            (TAC (..), genCode,
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
import           TACType

backend :: Program -> Dictionary -> IO ([TAC], NumberedBlocks, Graph, String)
backend program dictionary = do
    (_, _, code) <- runRWST (genCode program) dictionary initialState
    let optimizedCode = optimize code
    let basicBlocks = zip [0..] $ findBasicBlocks $ numberTACs optimizedCode
    let flowGraph = generateFlowGraph optimizedCode

    -- ! TEMP
    allocatedTac <- mapM mockAllocate $ findBasicBlocks $ numberTACs optimizedCode
    -- ! TEMP

    return (optimizedCode, basicBlocks, flowGraph, intercalate "\n" allocatedTac)

mockAllocate :: [TAC] ->  IO String
mockAllocate t = do
    (_, _, c) <- runRWST allocateRegisters t initialDescriptorLists
    return $ intercalate "\n" c