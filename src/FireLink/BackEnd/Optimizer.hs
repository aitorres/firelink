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
optimizations = [removeUnreachableJumps, removeUnusedLabels, removeRedundantJumps, removeIdempotencies, removeUnreachableInstructions]

-- | 'Optimization' that removes unreachable jumps, that is: a jump right next to a return statement
removeUnreachableJumps :: Optimization
removeUnreachableJumps = foldr removeUnreachableJump []
    where
        isUnreachableJump :: TAC -> TAC -> Bool
        isUnreachableJump (ThreeAddressCode Return _ _ _) (ThreeAddressCode GoTo _ _ _) = True
        isUnreachableJump _ _ = False

        removeUnreachableJump :: TAC -> [TAC] -> [TAC]
        removeUnreachableJump x [] = [x]
        removeUnreachableJump x seen =
            let nextTAC = head seen in
                if isUnreachableJump x nextTAC then x : tail seen else x : seen

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

-- | 'Optimization' that removes certain duplicated instructions that are
-- | idempotent, such as two sequential jumps to the same label,
-- | or two sequential returns (with the very same expression)
removeIdempotencies :: Optimization
removeIdempotencies = foldr removeIdempotency []
    where

        isIdempotentInstruction :: TAC -> TAC -> Bool
        isIdempotentInstruction (ThreeAddressCode GoTo _ _ a) (ThreeAddressCode GoTo _ _ b) = a == b
        isIdempotentInstruction (ThreeAddressCode Return _ a _) (ThreeAddressCode Return _ b _) = a == b
        isIdempotentInstruction (ThreeAddressCode Exit _ _ _) (ThreeAddressCode Exit _ _ _) = True
        isIdempotentInstruction (ThreeAddressCode Abort _ _ _) (ThreeAddressCode Abort _ _ _) = True
        isIdempotentInstruction _ _ = False

        removeIdempotency :: TAC -> [TAC] -> [TAC]
        removeIdempotency x [] = [x]
        removeIdempotency x seen =
            let nextTAC = head seen in
                if isIdempotentInstruction x nextTAC then seen else x:seen

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
