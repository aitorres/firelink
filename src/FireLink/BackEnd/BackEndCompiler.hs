module FireLink.BackEnd.BackEndCompiler (
    backend, Register(..), RegisterAssignment, InterferenceGraph, NumberedBlock, TAC(..), TACSymEntry(..),
    def, use
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
import           FireLink.BackEnd.FlowGraphGenerator        (FlowGraph,
                                                             NumberedBlock,
                                                             generateFlowGraph)
import           FireLink.BackEnd.InstructionCodeGenerator
import           FireLink.BackEnd.LivenessAnalyser
import           FireLink.BackEnd.Optimizer                 (optimize)
import           FireLink.BackEnd.RegisterAllocationProcess
import           FireLink.FrontEnd.Grammar                  (Program (..))
import           FireLink.FrontEnd.SymTable                 (Dictionary (..))
import           FireLink.Utils
import           TACType

import           Data.Map.Internal.Debug                    (showTree)

backend :: Program -> Dictionary -> IO (FlowGraph, InterferenceGraph, RegisterAssignment)
backend program dictionary = do
    (_, finalState, code) <- runRWST (genCode program) dictionary initialState
    let postProcessedCode = fillEmptyTemporals (cgsTemporalsToReplace finalState) code
    let optimizedCode = optimize postProcessedCode
    let flowGraph@(numberedBlocks, graph) = generateFlowGraph optimizedCode

    (registerAssignment, finalFlowGraph) <- run flowGraph dictionary
    let (interferenceGraph'', _) = generateInterferenceGraph' finalFlowGraph
    return (finalFlowGraph, interferenceGraph'', registerAssignment)
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

