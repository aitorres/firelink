{-|
Module      : FireLink.BackEnd.RegisterAllocationProcess
Description : Register allocation algorithm by graph coloration
Stability   : experimental

Attempts to allocate registers by doing chaitan algorithm as described here:
https://cs.gmu.edu/~white/CS640/p98-chaitin.pdf

[@initialStep@] process all codeblocks as we have infinite registers. Also, on function calls,
all live variables at that moment are going to be "stored" before them. That is done to
avoid that the interference graph will have edges between their own variables.
-}
module FireLink.BackEnd.RegisterAllocationProcess where

import qualified Data.Map                            as DM
import           FireLink.BackEnd.CodeGenerator      (TACSymEntry (..))
import           FireLink.BackEnd.FlowGraphGenerator (FlowGraph (..))

-- Semantic aliases
type Register = Int
type Color = Register
type SymEntryRegisterMap = DM.Map TACSymEntry Color

-- | Generate attempting final TAC assumming that we will have infinite registers i.e one register per actual variable
initialStep :: FlowGraph -> (SymEntryRegisterMap, FlowGraph)
initialStep (numberedBlocks, graph) = undefined
