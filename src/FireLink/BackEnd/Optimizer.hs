module FireLink.BackEnd.Optimizer (
    optimize
) where

import           FireLink.BackEnd.CodeGenerator (TAC (..))
import           TACType

-- | An 'Optimization' is a function that receives a list of TAC and
-- | returns a (hopefully optimized) list of TAC.
type Optimization = [TAC] -> [TAC]

-- | Composes all valid optimizations into one function to be applied
-- | to a generated three-address code list.
optimize :: Optimization
optimize = foldr (.) id optimizations

-- | A list with all currently valid optimizations.
optimizations :: [Optimization]
optimizations = [removeRedundantJumps, removeDuplicateGotos, removeUnusedLabels]

-- | 'Optimization' that removes redundant jumps, that is: a jump that goes to a
-- | label that is defined in the very next line.
removeRedundantJumps :: Optimization
removeRedundantJumps = foldr removeRedundantJump []
    where
        isRedundantJump :: TAC -> TAC -> Bool
        isRedundantJump (ThreeAddressCode GoTo _ _ (Just destiny)) (ThreeAddressCode NewLabel _ (Just jump) _) =
            show destiny == show jump
        isRedundantJump _ _ = False

        removeRedundantJump :: TAC -> [TAC] -> [TAC]
        removeRedundantJump x [] = [x]
        removeRedundantJump x seen =
            let nextTAC = head seen in
                if isRedundantJump x nextTAC then seen else x:seen

-- | 'Optimization' that removes duplicated go-to instructions that are
-- | listed one next to the other, pointing to the same label.
removeDuplicateGotos :: Optimization
removeDuplicateGotos = foldr removeDuplicateGoto []
    where
        isGoTo :: TAC -> Bool
        isGoTo (ThreeAddressCode tac _ _ _) = tac == GoTo

        isSameInstruction :: TAC -> TAC -> Bool
        isSameInstruction (ThreeAddressCode tac1 _ _ _) (ThreeAddressCode tac2 _ _ _) = tac1 == tac2

        removeDuplicateGoto :: TAC -> [TAC] -> [TAC]
        removeDuplicateGoto x [] = [x]
        removeDuplicateGoto x seen =
            let nextTAC = head seen in
                if isSameInstruction nextTAC x && isGoTo x then seen else x:seen

-- | 'Optimization' that removes labels that are not pointed-to anywhere in the code.
removeUnusedLabels :: Optimization
removeUnusedLabels tacs = filter removeLabel tacs
    where
        usedLabels :: [String]
        usedLabels = map getLabelValue $ filter itJumps tacs

        getLabelValue :: TAC -> String
        getLabelValue (ThreeAddressCode _ _ _ (Just (Label label))) = label
        getLabelValue (ThreeAddressCode _ _ (Just (Label label)) _) = label

        itJumps :: TAC -> Bool
        itJumps (ThreeAddressCode tac _ _ _) = tac `elem` [GoTo, Eq, Neq, Lt, Lte, Gt, Gte, Call]

        removeLabel :: TAC -> Bool
        removeLabel (ThreeAddressCode NewLabel _ (Just (Label b)) _) = b `elem` usedLabels
        removeLabel _ = True
