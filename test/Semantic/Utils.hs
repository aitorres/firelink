module Utils where

import qualified SymTable as ST
import Lexer (scanTokens)
import Test.Hspec
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

extractDictionary :: String -> IO ST.SymTable
extractDictionary program = do
    (_, d, _) <- extractSymTable program
    return d

extractCompoundFromExtra :: [ST.Extra] -> ST.Extra
extractCompoundFromExtra [] = error "The `extra` array doesn't have any `Compound` item"
extractCompoundFromExtra (s@(ST.Compound _ _): _) = s
extractCompoundFromExtra (_:ss) = extractCompoundFromExtra ss

extractCompoundRecFromExtra :: [ST.Extra] -> ST.Extra
extractCompoundRecFromExtra [] = error "The `extra` array doesn't have any `CompoundRec` item"
extractCompoundRecFromExtra (s@ST.CompoundRec{} : _) = s
extractCompoundRecFromExtra (_:ss) = extractCompoundRecFromExtra ss

extractRecursiveFromExtra :: [ST.Extra] -> ST.Extra
extractRecursiveFromExtra [] = error "The `extra` array doesn't have any `Recursive` item"
extractRecursiveFromExtra (s@ST.Recursive{} : _) = s
extractRecursiveFromExtra (_:ss) = extractRecursiveFromExtra ss

extractSimpleFromExtra :: [ST.Extra] -> ST.Extra
extractSimpleFromExtra [] = error "The `extra` array doesn't have any `Simple` item"
extractSimpleFromExtra (s@ST.Simple{} : _) = s
extractSimpleFromExtra (_:ss) = extractSimpleFromExtra ss

extractRecordFieldsFromExtra :: [ST.Extra] -> ST.Extra
extractRecordFieldsFromExtra [] = error "The `extra` array doesn't have any `Simple` item"
extractRecordFieldsFromExtra (s@ST.RecordFields{} : _) = s
extractRecordFieldsFromExtra (_:ss) = extractRecordFieldsFromExtra ss

runTestForInvalidProgram :: String -> IO ()
runTestForInvalidProgram program = do
    tokens <- scanTokens program
    RWS.runRWST (parse $ fromJust tokens) () ST.initialState `shouldThrow` anyException

