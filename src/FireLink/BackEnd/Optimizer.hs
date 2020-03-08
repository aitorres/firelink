module FireLink.BackEnd.Optimizer (
    optimize
) where

import           FireLink.BackEnd.CodeGenerator (TAC (..))
import           TACType

-- | An 'Optimization' is a function that receives a list of TAC and
-- | returns a (hopefully optimized) list of TAC.
type Optimization = [TAC] -> [TAC]

-- | Applies optimizations iteratively to the TAC list until
-- | stability is reached (i.e. until it converges)
-- | (src: https://stackoverflow.com/a/23924238)
optimize :: Optimization
optimize = until =<< ((==) =<<) $ optimize'

-- | Composes all valid optimizations into one function to be applied
-- | to a generated three-address code list.
optimize' :: Optimization
optimize' = foldr (.) id optimizations

-- | A list with all currently valid optimizations.
optimizations :: [Optimization]
optimizations = [removeUnusedLabels, removeRedundantJumps, removeDuplicateGotos, removeRedundantAssignments, removeUnreachableInstructions]

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

-- | 'Optimization' that removes unreachable instructions between a goto
-- | and the jumped label
removeUnreachableInstructions :: Optimization
removeUnreachableInstructions = foldr removeUnreachableInstruction []
    where
        isUnreachableInstruction :: TAC -> TAC -> TAC -> Bool
        isUnreachableInstruction (ThreeAddressCode GoTo _ _ (Just destiny)) _ (ThreeAddressCode NewLabel _ (Just jump) _) =
            show destiny == show jump
        isUnreachableInstruction _ _ _ = False

        removeUnreachableInstruction :: TAC -> [TAC] -> [TAC]
        removeUnreachableInstruction x [] = [x]
        removeUnreachableInstruction x [y] = [x, y]
        removeUnreachableInstruction x seen =
            let middleTAC = head seen
                lastTAC = head $ tail seen in
                if isUnreachableInstruction x middleTAC lastTAC then x : tail seen else x:seen

-- | 'Optimization' that removes redundant assignments, that is, a pair
-- | of instructions in which the first performs an operation to var A, and
-- | the second stores the content of var A to var B (instead of storing directly
-- | to var B)
removeRedundantAssignments :: Optimization
removeRedundantAssignments = foldr removeRedundantAssignment []
    where
        isRedundantAssignment :: TAC -> TAC -> Bool
        isRedundantAssignment (ThreeAddressCode _ (Just c) _ _) (ThreeAddressCode Assign (Just a) (Just b) Nothing) = b == c
        isRedundantAssignment _ _ = False

        removeRedundantAssignment :: TAC -> [TAC] -> [TAC]
        removeRedundantAssignment x [] = [x]
        removeRedundantAssignment x@(ThreeAddressCode op _ b c) seen =
            let nextTAC@(ThreeAddressCode _ a _ _) = head seen in
                if isRedundantAssignment x nextTAC then ThreeAddressCode op a b c : tail seen else x : seen

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
