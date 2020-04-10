module FireLink.BackEnd.FlowGraphGenerator(
    generateFlowGraph, BasicBlock, NumberedBlock
) where

import           Data.Char                      (isDigit)
import           Data.Graph
import           Data.List                      (nub)
import           Data.Maybe
import           FireLink.BackEnd.CodeGenerator (OperandType (..), TAC (..),
                                                 isConditionalJump, isJump,
                                                 isProgramEnd,
                                                 isUnconditionalJump)
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

-- | FlowGraph representation. First tuple contains the basic blocks, with their respective number
-- | Second tuple position contains the graph with the basic block numbers as edges
type FlowGraph = (NumberedBlocks, Graph)

-- | Generates and returns the flow graph
-- | that correspond to a given list of TAC instructions.
generateFlowGraph :: [TAC] -> FlowGraph
generateFlowGraph code =
    let numberedInstructions = numberTACs code
        basicBlocks = findBasicBlocks numberedInstructions
        numberedBlocks = numberBlocks basicBlocks
        fallEdges = getFallEdges numberedBlocks
        entryEdge = (-1, 0) -- ENTRY
        exitNode = length numberedBlocks -- EXIT
        jumpEdges = getJumpEdges numberedBlocks exitNode
        edges = entryEdge : jumpEdges ++ fallEdges
        graph = buildG (-1, length numberedBlocks) edges
    in  (numberedBlocks, graph)

-- | Given an integer that represents an EXIT vertex, and a
-- | list of numbered blocks, returns a list of edges
-- | from blocks that end up in jumps to their jumped blocks,
-- | including jumps from exit blocks to the EXIT vertex
getJumpEdges :: NumberedBlocks -> Vertex -> Edges
getJumpEdges code vExit = foldr findAllJumpEdges [] code
    where
        findAllJumpEdges :: NumberedBlock -> Edges -> Edges
        findAllJumpEdges (i, cb) es =
            let lastInstr = last cb
                ThreeAddressCode op _ dC dJ = lastInstr
            in  if isConditionalJump op || op == GoTo
                then let (j, _) = findDestinyBlock dJ code
                    in  (i, j) : es
                else if isProgramEnd op then (i, vExit) : es
                else if op == Call then
                    let (j, _) = findDestinyBlock dC code
                    in (i, j) : es
                else if op == Return then
                    let (_, fcb) = findFunctionDefBlock i code
                        ThreeAddressCode _ _ mL _ = head fcb
                    in  es ++ map (\x -> (i, x)) (getCallsToLabel mL code)
                else es

        findDestinyBlock :: Maybe OperandType -> NumberedBlocks -> NumberedBlock
        findDestinyBlock d = head . filter (\(_, b) -> head b == ThreeAddressCode NewLabel Nothing d Nothing)

        findFunctionDefBlock :: Vertex -> NumberedBlocks -> NumberedBlock
        findFunctionDefBlock i = last . filter (\(i', cb) -> i' <= i && (isFuncLabel . head) cb)

        isFuncLabel :: TAC -> Bool
        isFuncLabel (ThreeAddressCode NewLabel _ (Just (Label s)) _) = let sc = head s in (not . isDigit) sc
        isFuncLabel _ = False

        getCallsToLabel :: Maybe OperandType -> NumberedBlocks -> [Vertex]
        getCallsToLabel Nothing _ = []
        getCallsToLabel (Just l) code =
            let blocksEndingWithCall = filter (endsWithCallToLabel l) code
            -- We add 1 because we should return to the block immediately following this one
            in  map ((+1) . fst) blocksEndingWithCall

        endsWithCallToLabel :: OperandType -> NumberedBlock -> Bool
        endsWithCallToLabel l (_, tacs) = let lastInstr = last tacs in isCallToLabel lastInstr l

        isCallToLabel :: TAC -> OperandType -> Bool
        isCallToLabel (ThreeAddressCode Call _ (Just l) _) l' = l == l'
        isCallToLabel _  _                                    = False

-- | Given a list of numbered blocks, finds all fall edges, that is,
-- | edges from a block to its following block (fall-through edges)
-- | where applicable (i.e. if there are no unconditional jumps
-- | at the end of the first block)
getFallEdges :: NumberedBlocks -> Edges
getFallEdges [] = []
getFallEdges [x] = []
getFallEdges (x:ys@(y:_)) = case hasFallEdge x y of
    Nothing -> getFallEdges ys
    Just e  -> e : getFallEdges ys
    where
        hasFallEdge :: NumberedBlock -> NumberedBlock -> Maybe Edge
        hasFallEdge (i1, t1) (i2, _) =
            let ThreeAddressCode op _ _ _ = last t1
            in  if isUnconditionalJump op || isProgramEnd op then Nothing else Just (i1, i2)

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
        isJumpTac (_, ThreeAddressCode op _ _ _) = isJump op

        isJumpDestiny :: NumberedTAC -> Bool
        isJumpDestiny (_, ThreeAddressCode NewLabel _ (Just _) _) = True
        isJumpDestiny _                                           = False

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
