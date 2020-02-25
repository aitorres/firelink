module FireLink.BackEnd.BackEndCompiler (
    backend
) where

import           Control.Monad.RWS                         (runRWST)
import           FireLink.BackEnd.CodeGenerator            (TAC (..), genCode,
                                                            initialState)
import           FireLink.BackEnd.InstructionCodeGenerator ()
import           FireLink.FrontEnd.Grammar                 (Program (..))
import           FireLink.FrontEnd.SymTable                (Dictionary (..))
import           TACType

backend :: Program -> Dictionary -> IO [TAC]
backend program dictionary = do
    (_, _, code) <- runRWST (genCode program) dictionary initialState
    return $ (removeDuplicateGotos . removeUnusedLabels) code

removeDuplicateGotos :: [TAC] -> [TAC]
removeDuplicateGotos = foldr removeDuplicateGoto []
    where
        isGoTo :: TAC -> Bool
        isGoTo (ThreeAddressCode tac _ _ _) = tac == GoTo

        isSameInstruction :: TAC -> TAC -> Bool
        isSameInstruction (ThreeAddressCode tac1 _ _ _) (ThreeAddressCode tac2 _ _ _) = tac1 == tac2

        removeDuplicateGoto :: TAC -> [TAC] -> [TAC]
        removeDuplicateGoto x [] = [x]
        removeDuplicateGoto x seen =
            let mostRecentTAC = head seen in
                if isSameInstruction mostRecentTAC x && isGoTo x then seen else x:seen

removeUnusedLabels :: [TAC] -> [TAC]
removeUnusedLabels tacs = filter removeLabel tacs
    where
        usedLabels :: [String]
        usedLabels = map getLabelValue $ filter itJumps tacs

        getLabelValue :: TAC -> String
        getLabelValue (ThreeAddressCode _ _ _ (Just (Label label))) = label

        itJumps :: TAC -> Bool
        itJumps (ThreeAddressCode tac _ _ _) = tac `elem` [GoTo, Eq, Neq, Lt, Lte, Gt, Gte]

        removeLabel :: TAC -> Bool
        removeLabel (ThreeAddressCode NewLabel _ (Just (Label b)) _) = b `elem` usedLabels
        removeLabel _ = True
