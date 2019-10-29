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


type TestFunction a b
    = a
    -> ST.DictionaryEntry
    -> ([ST.Extra] -> ST.Extra)
    -> (ST.Extra -> Bool)
    -> IO b

testEntry :: TestFunction ST.Dictionary ()
testEntry dict expectedEntry extractor predicate = do
    let varName = ST.name expectedEntry
    let chain = filter (\d -> ST.name d == varName) $ ST.findChain varName dict
    chain `shouldNotSatisfy` null
    let actualEntry = head chain
    ST.name actualEntry `shouldBe` varName
    ST.category actualEntry `shouldBe` ST.category expectedEntry
    ST.scope actualEntry `shouldBe` ST.scope expectedEntry
    ST.entryType actualEntry `shouldBe` ST.entryType expectedEntry
    let extra' = ST.extra actualEntry
    extra' `shouldNotSatisfy` null
    extractor extra' `shouldSatisfy` predicate

test :: TestFunction String ST.Dictionary
test prog expectedEntry extractor predicate = do
    (_, (dict, _, _), _) <- extractSymTable prog
    testEntry dict expectedEntry extractor predicate
    return dict

testVoid :: TestFunction String ()
testVoid prog expectedEntry extractor predicate = RWS.void $ test prog expectedEntry extractor predicate
