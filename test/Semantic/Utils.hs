module Utils where

import qualified SymTable as ST
import Lexer (scanTokens)
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
    RWS.runRWST (parse $ fromJust tokens) () ST.initialState

extractSizeFromExtra :: [ST.Extra] -> G.Expr
extractSizeFromExtra [] = error "The `extra` array doesn't have any `ConstructedBy e` item"
extractSizeFromExtra (ST.Size e : _) = e
extractSizeFromExtra (_:ss) = extractSizeFromExtra ss

extractConstructorFromExtra :: [ST.Extra] -> ST.DictionaryEntry
extractConstructorFromExtra [] = error "The `extra` array doesn't have any `ConstructedBy e` item"
extractConstructorFromExtra (ST.ConstructedBy s : _) = s
extractConstructorFromExtra (_:ss) = extractConstructorFromExtra ss

runTestForInvalidProgram :: String -> IO ()
runTestForInvalidProgram program = do
    tokens <- scanTokens program
    RWS.runRWST (parse $ fromJust tokens) () ST.initialState `TH.shouldThrow` TH.anyException

