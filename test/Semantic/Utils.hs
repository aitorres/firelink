module Utils where

import qualified SymTable as ST
import Lexer (Tokens, scanTokens)
import qualified Test.Hspec as TH
import Data.Maybe (fromJust)
import Parser
import qualified Grammar as G
import qualified Control.Monad.RWS as RWS

extractSymTable
    :: String
    -> IO (G.Program, ST.SymTable, ST.SemanticErrors)
extractSymTable program = do
    tokens <- scanTokens program
    a <- RWS.runRWST (parse $ fromJust tokens) () ST.initialState
    return a

runTestForInvalidProgram :: String -> IO ()
runTestForInvalidProgram program = do
    tokens <- scanTokens program
    RWS.runRWST (parse $ fromJust tokens) () ST.initialState `TH.shouldThrow` TH.anyException

