module FireLink.BackEnd.FlowGraphGenerator (
    generateFlowGraph, findBasicBlocks, numberTACs
) where

import           Data.Graph
import           Data.List                      (nub)
import           Data.Maybe
import           FireLink.BackEnd.CodeGenerator (TAC (..), isInconditionalJump,
                                                 isJump)
import           TACType

-- | A numbered instruction (the number being a unique identifier within some context)
type NumberedTAC = (Int, TAC)

-- | Semantic shortcut for a list of numbered instructions
type NumberedTACs = [NumberedTAC]

-- | A group of TAC within a basic context
type BasicBlock = [TAC]

-- | Semantic shortcut for a list of basic blocks
type BasicBlocks = [BasicBlock]

-- | A numbered TAC block (the number being a unique identifier within some context)
type NumberedBlock = (Int, BasicBlock)

-- | Semantic shortcut for a list of numbered blocks
type NumberedBlocks = [NumberedBlock]

-- | Semantic shortcut for a list of edges
type Edges = [Edge]

-- | Generates and returns the flow graph
-- | that correspond to a given list of TAC instructions.
generateFlowGraph :: [TAC] -> Graph
generateFlowGraph code =
    let numberedInstructions = numberTACs code
        basicBlocks = findBasicBlocks numberedInstructions
        numberedBlocks = numberBlocks basicBlocks
        directEdges = getDirectEdges numberedBlocks
        entryEdge = (-1, 0) -- ENTRY
        exitEdges = getExitEdges (length numberedBlocks) numberedBlocks
        edges = entryEdge : directEdges ++ exitEdges
        graph = buildG (-1, length numberedBlocks) edges
    in  graph

-- | Given an integer that represents an EXIT vertex, and a
-- | list of numbered blocks, returns a list of edges from
-- | exit blocks to the EXIT vertex
getExitEdges :: Vertex -> NumberedBlocks -> Edges
getExitEdges vExit = foldr addExitEdges []
    where
        addExitEdges :: NumberedBlock -> Edges -> Edges
        addExitEdges (i, cb) es =
            let lastInstr = last cb
                ThreeAddressCode op _ _ _ = lastInstr
            in  if isExitOp op then (i, vExit) : es else es

        isExitOp :: Operation -> Bool
        isExitOp = flip elem [Exit, Abort]

getDirectEdges :: NumberedBlocks -> Edges
getDirectEdges [] = []
getDirectEdges [x] = []
getDirectEdges (x:ys@(y:_)) = case hasDirectEdge x y of
    Nothing -> getDirectEdges ys
    Just e  -> e : getDirectEdges ys
    where
        hasDirectEdge :: NumberedBlock -> NumberedBlock -> Maybe Edge
        hasDirectEdge (i1, t1) (i2, _) =
            let ThreeAddressCode op _ _ _ = last t1
            in  if isInconditionalJump op then Nothing else Just (i1, i2)

-- | Given a list of numbered TAC instructions, returns a list with
-- | their basic blocks
findBasicBlocks :: NumberedTACs -> BasicBlocks
findBasicBlocks t = let leaders = findBlockLeaders t in removeLineTags $ findBasicBlocks' t leaders
    where
        findBasicBlocks' :: NumberedTACs -> NumberedTACs -> [NumberedTACs]
        findBasicBlocks' code leaders = groupBasicBlocks code (tail leaders) []

        groupBasicBlocks :: NumberedTACs -> NumberedTACs -> NumberedTACs -> [NumberedTACs]
        -- If I already recognized the last leader, then all remaining code is just one basic block
        groupBasicBlocks code [] [] = [code]
        groupBasicBlocks code@(c:cs) leaders@(l:ls) prevCode
            | c == l = prevCode : groupBasicBlocks code ls []
            | otherwise = groupBasicBlocks cs leaders (prevCode ++ [c])

        removeLineTags :: [NumberedTACs] -> BasicBlocks
        removeLineTags = map (map snd)

-- | Given a list of numbered TAC instructions, finds and returns
-- | a list of block leaders:
-- |   1. The first instruction (appended to the result of the aux function)
-- |   2. The destiny of each jump (workaround: since we only leave reachable labels, those are added)
-- |   3. The next instruction after each jump/goto
-- | Conditions (2) and (3) are handled in the recursive, aux function
findBlockLeaders :: NumberedTACs -> NumberedTACs
findBlockLeaders ts = nub $ head ts : findBlockLeaders' ts
    where
        findBlockLeaders' :: NumberedTACs -> NumberedTACs
        findBlockLeaders' [] = []
        findBlockLeaders' [x] = []
        findBlockLeaders' (x:ys@(y:_)) = [y | isJumpTac x] ++ [x | isJumpDestiny x] ++ findBlockLeaders' ys

        isJumpTac :: NumberedTAC -> Bool
        isJumpTac (_, (ThreeAddressCode op _ _ _)) = isJump op

        isJumpDestiny :: NumberedTAC -> Bool
        isJumpDestiny (_, (ThreeAddressCode NewLabel _ (Just _) _)) = True
        isJumpDestiny _                                             = False

-- | Type-binding application of numberList for a code block
numberTACs :: [TAC] -> NumberedTACs
numberTACs = numberList

-- | Type-binding application of numberList for a list of code blocks
numberBlocks :: BasicBlocks -> NumberedBlocks
numberBlocks = numberList

-- | Given a list, returns a list of pairs in which
-- | each first component is an unique integer, and each second
-- | component is a (now uniquely identified) element of the first list,
-- | in the original order
numberList :: [a] -> [(Int, a)]
numberList = zip [0..]
