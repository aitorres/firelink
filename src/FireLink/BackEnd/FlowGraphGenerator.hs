module FireLink.BackEnd.FlowGraphGenerator (
    generateFlowGraph, findBasicBlocks
) where

import           Data.Graph
import           FireLink.BackEnd.CodeGenerator (TAC (..))
import           TACType

-- | Generates and returns the flow graph
-- | that correspond to a given list of TAC instructions.
generateFlowGraph :: [TAC] -> [(Int, [TAC])]
generateFlowGraph code =
    let basicBlocks = findBasicBlocks code
        numberedBlocks = numberBasicBlocks basicBlocks
    in  numberedBlocks

-- | Given a list of basic blocks, returns a list of pairs in which
-- | each first component is an unique integer, and each second
-- | component is a (now uniquely identified) basic block
numberBasicBlocks :: [[TAC]] -> [(Int, [TAC])]
numberBasicBlocks = zip [0..]

-- | Given a list of TAC instructions, returns a list with
-- | their basic blocks
findBasicBlocks :: [TAC] -> [[TAC]]
findBasicBlocks t = findBasicBlocks' t $ findBlockLeaders t

-- | Given a list of TAC instructions, and the previously found leaders
-- | of said list, return a list with the basic blocks of the TAC instructions
findBasicBlocks' :: [TAC] -> [TAC] -> [[TAC]]
findBasicBlocks' code leaders = findBasicBlocks' code leaders []
    where
        findBasicBlocks' :: [TAC] -> [TAC] -> [TAC] -> [[TAC]]
        -- If I only have one remaining leader, then all remaining code is just one basic block
        findBasicBlocks' code [_] [] = [code]
        findBasicBlocks' code@(c:cs) leaders@(l:ls) prevCode
            | c == l = if null prevCode then nextBasicBlock else prevCode : nextBasicBlock
            | otherwise = findBasicBlocks' cs leaders (prevCode ++ [c])
            where
                nextBasicBlock = findBasicBlocks' code ls []

-- | Given a list of TAC instructions, finds and returns
-- | a list of block leaders:
-- |   1. The first instruction (base case for the folded function)
-- |   2. The destiny of each jump (workaround: since we only leave reachable labels, those are added)
-- |   3. The next instruction after each jump/goto
findBlockLeaders ::[TAC] -> [TAC]
findBlockLeaders = foldr isBlockLeader []
    where
        isJumpDestiny :: TAC -> Bool
        isJumpDestiny (ThreeAddressCode NewLabel _ (Just _) _) = True
        isJumpDestiny _                                        = False

        isGoTo :: TAC -> Bool
        isGoTo (ThreeAddressCode GoTo _ _ _) = True
        isGoTo _                             = False

        isBlockLeader :: TAC -> [TAC] -> [TAC]
        isBlockLeader x [] = [x] -- Handles the first instruction which is always a leader (1)
        isBlockLeader x leaders
            | isJumpDestiny x = x : leaders -- Reached jump destinies (2)
            | isGoTo x = x : leaders -- Workaround to find actual instructions after jumps (see next line)
            | isGoTo lastInstr = x : tail leaders -- Instructions immediately after a jump (3)
            | otherwise = leaders
            where
                lastInstr = head leaders
