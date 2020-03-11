module FireLink.BackEnd.FlowGraphGenerator (
    generateFlowGraph, findBasicBlocks, findBlockLeaders
) where

import           Data.Graph
import           Data.List                      (nub)
import           Data.Maybe
import           FireLink.BackEnd.CodeGenerator (TAC (..), isInconditionalJump,
                                                 isJump)
import           TACType

type NumberedBlock = (Int, [TAC])
type NumberedBlocks = [NumberedBlock]
type Edges = [Edge]

-- | Generates and returns the flow graph
-- | that correspond to a given list of TAC instructions.
-- | TODO: While finding leaders and basic blocks, number each instruction in order to distinguish
-- | leaders that might belong to the same instructions, or a same instruction being both leader and not leader
generateFlowGraph :: [TAC] -> Graph
generateFlowGraph code =
    let basicBlocks = findBasicBlocks code
        numberedBlocks = numberBasicBlocks basicBlocks
        directEdges = getDirectEdges numberedBlocks
        entryEdge = (-1, 0) -- ENTRY
        edges = entryEdge : directEdges
        graph = buildG (-1, length numberedBlocks - 1) edges
    in  graph

getDirectEdges :: NumberedBlocks -> Edges
getDirectEdges [] = []
getDirectEdges [x] = []
getDirectEdges (x:ys@(y:_)) = case hasDirectEdge x y of
    Nothing -> getDirectEdges ys
    Just e  -> e : getDirectEdges ys
    where
        hasDirectEdge :: NumberedBlock -> NumberedBlock -> Maybe Edge
        hasDirectEdge (i1, t1) (i2, _) =
            let (ThreeAddressCode op _ _ _) = last t1
            in  if isInconditionalJump op then Nothing else Just (i1, i2)

-- | Given a list of basic blocks, returns a list of pairs in which
-- | each first component is an unique integer, and each second
-- | component is a (now uniquely identified) basic block
numberBasicBlocks :: [[TAC]] -> NumberedBlocks
numberBasicBlocks = zip [0..]

-- | Given a list of TAC instructions, returns a list with
-- | their basic blocks
findBasicBlocks :: [TAC] -> [[TAC]]
findBasicBlocks t = let leaders = findBlockLeaders t in findBasicBlocks' t leaders

-- | Given a list of TAC instructions, and the previously found leaders
-- | of said list, return a list with the basic blocks of the TAC instructions
findBasicBlocks' :: [TAC] -> [TAC] -> [[TAC]]
findBasicBlocks' code leaders = findBasicBlocks'' code (tail leaders) []
    where
        findBasicBlocks'' :: [TAC] -> [TAC] -> [TAC] -> [[TAC]]
        -- If I already recognized the last leader, then all remaining code is just one basic block
        findBasicBlocks'' code [] [] = [code]
        findBasicBlocks'' code@(c:cs) leaders@(l:ls) prevCode
            | c == l = prevCode : findBasicBlocks'' code ls []
            | otherwise = findBasicBlocks'' cs leaders (prevCode ++ [c])

-- | Given a list of TAC instructions, finds and returns
-- | a list of block leaders:
-- |   1. The first instruction (appended to the result of the aux function)
-- |   2. The destiny of each jump (workaround: since we only leave reachable labels, those are added)
-- |   3. The next instruction after each jump/goto
-- | Conditions (2) and (3) are handled in the recursive, aux function
findBlockLeaders :: [TAC] -> [TAC]
findBlockLeaders ts = nub $ head ts : findBlockLeaders' ts
    where
        findBlockLeaders' :: [TAC] -> [TAC]
        findBlockLeaders' [] = []
        findBlockLeaders' [x] = []
        findBlockLeaders' (x:ys@(y:_)) = [y | isJumpTac x] ++ [x | isJumpDestiny x] ++ findBlockLeaders' ys

        isJumpTac :: TAC -> Bool
        isJumpTac (ThreeAddressCode op _ _ _) = isJump op

        isJumpDestiny :: TAC -> Bool
        isJumpDestiny (ThreeAddressCode NewLabel _ (Just _) _) = True
        isJumpDestiny _                                        = False
