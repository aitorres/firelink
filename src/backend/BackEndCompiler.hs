module BackEndCompiler (
    backend
) where

import           CodeGenerator            (TAC (..), genCode, initialState)
import           Control.Monad.RWS        (runRWST)
import           Grammar                  (Program (..))
import           InstructionCodeGenerator ()
import           SymTable                 (Dictionary (..))
import           TACType

backend :: Program -> Dictionary -> IO [TAC]
backend program dictionary = do
    (_, _, code) <- runRWST (genCode program) dictionary initialState
    return code

removeUnusedLabels :: [TAC] -> [TAC]
removeUnusedLabels tacs = filter removeLabel tacs
    where
        usedLabels :: [Int]
        usedLabels = map getLabelValue $ filter itJumps tacs

        getLabelValue :: TAC -> Int
        getLabelValue (ThreeAddressCode _ _ _ (Just (Label label))) = label

        itJumps :: TAC -> Bool
        itJumps (ThreeAddressCode tac _ _ _) = tac `elem` [GoTo, Eq, Neq, Lt, Lte, Gt, Gte]

        removeLabel :: TAC -> Bool
        removeLabel (ThreeAddressCode NewLabel _ (Just (Label b)) _) = b `elem` usedLabels
        removeLabel _ = True
