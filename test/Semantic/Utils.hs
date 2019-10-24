module Utils where

import qualified SymTable as ST
import Lexer (Tokens, scanTokens)
import qualified Test.Hspec as TH
import Data.Maybe (fromJust)
import Parser
import Grammar
import qualified Control.Monad.RWS as RWS

extractSymTable program predicate = do
    tokens <- scanTokens program
    (ast, _, _) <- RWS.runRWST (parse $ fromJust tokens) () ST.initialState
    ast `TH.shouldSatisfy` predicate

runTestForInvalidProgram program = do
    tokens <- scanTokens program
    RWS.runRWST (parse $ fromJust tokens) () ST.initialState `TH.shouldThrow` TH.anyException
