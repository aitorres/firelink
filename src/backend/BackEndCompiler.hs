module BackEndCompiler where

import Grammar (Program(..))
import SymTable (Dictionary(..))
import CodeGenerator (initialState, TAC(..), genCode)
import InstructionCodeGenerator ()
import Control.Monad.RWS (runRWST)
import TACType

backend :: Program -> Dictionary -> IO [TAC]
backend program dictionary = do
    (_, _, code) <- runRWST (genCode program) dictionary initialState
    return $ removeUnusedLabels code

removeUnusedLabels :: [TAC] -> [TAC]
removeUnusedLabels tacs = filter removeLabel tacs
    where
        usedLabels :: [Int]
        usedLabels = map getLabelValue $ filter isGoTo tacs

        getLabelValue :: TAC -> Int
        getLabelValue (ThreeAddressCode GoTo Nothing Nothing (Just (Label label))) = label

        isGoTo :: TAC -> Bool
        isGoTo (ThreeAddressCode GoTo _ _ _) = True
        isGoTo _ = False

        removeLabel :: TAC -> Bool
        removeLabel (ThreeAddressCode NewLabel _ (Just (Label b)) _) = b `elem` usedLabels
        removeLabel _ = True
