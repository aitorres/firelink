module FireLink.BackEnd.FlowGraphGenerator (
    generateFlowGraph
) where

import           FireLink.BackEnd.CodeGenerator (TAC (..))
import           TACType

-- | Generates and returns the flow graph
-- | that correspond to a given list of TAC instructions.
generateFlowGraph :: [TAC] -> [[TAC]]
generateFlowGraph code = let leaders = findBlockLeaders code in findBasicBlocks code leaders

-- | Given a list of TAC instructions, and the previously found leaders
-- | of said list, return a list with the basic blocks of the TAC instructions
findBasicBlocks :: [TAC] -> [TAC] -> [[TAC]]
findBasicBlocks code leaders = findBasicBlocks' code leaders []
    where
        findBasicBlocks' :: [TAC] -> [TAC] -> [TAC] -> [[TAC]]
        findBasicBlocks' code@(c:cs) [l] prevCode
            | c == l = [prevCode ++ code]
            | otherwise = findBasicBlocks' cs [l] (prevCode ++ [c])
        findBasicBlocks' code@(c:cs) leaders@(l:ls) prevCode
            | c == l = if null prevCode then findBasicBlocks' code ls [] else prevCode : findBasicBlocks' code ls []
            | otherwise = findBasicBlocks' cs leaders (prevCode ++ [c])

-- | Given a list of TAC instructions, finds and returns
-- | a list of block leaders:
-- |   1. The first instruction (base case for the folded function)
-- |   2. The destiny of each jump (workaround: since we only leave reachable labels, those are added)
-- |   3. The next instruction after each jump/goto
findBlockLeaders ::[TAC] -> [TAC]
findBlockLeaders = foldl isBlockLeader []
    where
        isJumpDestiny :: TAC -> Bool
        isJumpDestiny (ThreeAddressCode NewLabel _ (Just _) _) = True
        isJumpDestiny _                                        = False

        isGoTo :: TAC -> Bool
        isGoTo (ThreeAddressCode GoTo _ _ _) = True
        isGoTo _                             = False

        isBlockLeader :: [TAC] -> TAC -> [TAC]
        isBlockLeader [] x = [x] -- Handles the first instruction which is always a leader (1)
        isBlockLeader leaders x
            | isJumpDestiny x = leaders ++ [x] -- Reached jump destinies (2)
            | isGoTo x = leaders ++ [x] -- Workaround to find actual instructions after jumps (see next line)
            | isGoTo lastInstr = init leaders ++ [x] -- Instructions immediately after a jump (3)
            | otherwise = leaders
            where
                lastInstr = last leaders
