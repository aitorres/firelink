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
-- removeUnusedLabels t = t
removeUnusedLabels tacs = filter removeLabel tacs
    where
        usedLabels :: [Int]
        usedLabels = map getLabelValue $ filter isGoTo tacs

        getLabelValue :: TAC -> Int
        getLabelValue (ThreeAddressCode _ _ _ (Just (Label label))) = label

        isGoTo :: TAC -> Bool
        isGoTo (ThreeAddressCode tac _ _ _) = tac `elem` [GoTo, Eq, Neq, Lt, Lte, Gt, Gte]

        removeLabel :: TAC -> Bool
        removeLabel (ThreeAddressCode NewLabel _ (Just (Label b)) _) = b `elem` usedLabels
        removeLabel _ = True
