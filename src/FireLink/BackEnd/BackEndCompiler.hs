module FireLink.BackEnd.BackEndCompiler (
    backend
) where

import           Control.Monad.RWS                         (runRWST)
import           Data.Graph                                (Graph)
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

backend :: Program -> Dictionary -> IO ([TAC], NumberedBlocks, Graph)
backend program dictionary = do
    (_, _, code) <- runRWST (genCode program) dictionary initialState
    let optimizedCode = optimize code
    let basicBlocks = zip [0..] $ findBasicBlocks $ numberTACs optimizedCode
    let flowGraph = generateFlowGraph optimizedCode
    return (optimizedCode, basicBlocks, flowGraph)
