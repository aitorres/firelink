{-|
Module      : FireLink.BackEnd.RegisterAllocationProcess
Description : Register allocation algorithm by graph coloration
Stability   : experimental

Attempts to allocate registers by doing chaitan algorithm as described here:
https://cs.gmu.edu/~white/CS640/p98-chaitin.pdf

[@initialStep@] process all codeblocks as we have infinite registers. Also, on function calls,
all live variables at that moment are going to be "stored" before them. That is done to
avoid that the interference graph will have edges between their own variables.
-}
module FireLink.BackEnd.RegisterAllocationProcess (initialStep) where

import           Data.Char                           (isDigit)
import qualified Data.Map                            as DM
import qualified Data.Set                            as DS
import           Debug.Trace                         (trace)
import           FireLink.BackEnd.CodeGenerator      (TAC (..),
                                                      TACSymEntry (..))
import           FireLink.BackEnd.FlowGraphGenerator (BasicBlock,
                                                      FlowGraph (..),
                                                      NumberedBlock,
                                                      getProgramVariables)
import           FireLink.BackEnd.LivenessAnalyser
import           FireLink.FrontEnd.SymTable          (Dictionary (..),
                                                      findArgsByFunName,
                                                      getOffset)
import qualified FireLink.FrontEnd.SymTable          as ST (DictionaryEntry (..))
import           TACType

-- Semantic aliases
type Register = Int
type Color = Register
type SymEntryRegisterMap = DM.Map TACSymEntry Color

-- | Generate attempting final TAC assumming that we will have infinite registers i.e one register per actual variable
-- | Also, before and after functions call live variables at that point will be saved to and loaded from memory respectively.
-- | This is done to reduce the size of the interference graph, and because variables from different functions are not supposed
-- | to interfere between them.
initialStep :: FlowGraph -> Dictionary -> (SymEntryRegisterMap, FlowGraph)
initialStep flowGraph@(numberedBlocks, graph) dict =
    (variableRegisterMap, (preProcessCode, graph))
    where
        programVariables :: DS.Set TACSymEntry
        programVariables = getProgramVariables numberedBlocks

        -- | one for each actual variable
        initialRegisters :: [Register]
        initialRegisters = [0 .. DS.size programVariables - 1]

        -- | Map for each tac to its initial register
        variableRegisterMap :: SymEntryRegisterMap
        variableRegisterMap = DM.fromList $ zip (DS.toList programVariables) initialRegisters

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
        go' (programPoint, tac@(ThreeAddressCode Call _ _ _)) =
            let liveVariables = llvInLiveVariables $ getLivenessIn programPoint
                storeTacs = map (\tacSymEntry ->
                    ThreeAddressCode Store (Just (Id tacSymEntry)) Nothing Nothing) $ DS.toList liveVariables
                in storeTacs ++ [tac]

        -- after generating code for labels, we need to load the used-arguments to their registers.
        -- if a var is not in the variableRegisterMap then it is not used in the program i.e. we can
        -- skip its load
        go' (_, tac@(ThreeAddressCode NewLabel _ (Just (Label funName)) _)) =
            if isTacLabelFun tac then
                let args = getFunctionArguments funName
                    loadTacsSet = map (\tacSymEntry ->
                        case variableRegisterMap DM.!? tacSymEntry of
                            Nothing -> []
                            Just _ -> [ThreeAddressCode Load (Just (Id tacSymEntry)) Nothing Nothing]) args
                    loadTacs = concat loadTacsSet in
                        tac : loadTacs
            else [tac]
        go' (_, tac) = [tac]
