module TestUtils where

import qualified Control.Monad.RWS          as RWS
import           FireLink.FrontEnd.Errors
import qualified FireLink.FrontEnd.Grammar  as G
import qualified FireLink.FrontEnd.Lexer    as L
import           FireLink.FrontEnd.Parser
import qualified FireLink.FrontEnd.SymTable as ST
import           FireLink.Utils
import           Test.Hspec

extractSymTable
    :: String
    -> IO (G.Program, ST.SymTable, [Error])
extractSymTable program = do
    let ([], tokens) = L.scanTokens program
    RWS.runRWST (parse tokens) () ST.initialState

extractDictionary :: String -> IO ST.SymTable
extractDictionary program = do
    (_, d, _) <- extractSymTable program
    return d

extractErrors :: String -> IO [Error]
extractErrors program = do
    (_, _, e) <- extractSymTable program
    return e

type Extractor = [ST.Extra] -> ST.Extra
extractCompoundFromExtra :: Extractor
extractCompoundFromExtra [] = error "The `extra` array doesn't have any `Compound` item"
extractCompoundFromExtra (s@(ST.Compound _ _): _) = s
extractCompoundFromExtra (_:ss) = extractCompoundFromExtra ss

extractCompoundRecFromExtra :: Extractor
extractCompoundRecFromExtra [] = error "The `extra` array doesn't have any `CompoundRec` item"
extractCompoundRecFromExtra (s@ST.CompoundRec{} : _) = s
extractCompoundRecFromExtra (_:ss) = extractCompoundRecFromExtra ss

extractRecursiveFromExtra :: Extractor
extractRecursiveFromExtra [] = error "The `extra` array doesn't have any `Recursive` item"
extractRecursiveFromExtra (s@ST.Recursive{} : _) = s
extractRecursiveFromExtra (_:ss) = extractRecursiveFromExtra ss

extractSimpleFromExtra :: Extractor
extractSimpleFromExtra [] = error "The `extra` array doesn't have any `Simple` item"
extractSimpleFromExtra (s@ST.Simple{} : _) = s
extractSimpleFromExtra (_:ss) = extractSimpleFromExtra ss

extractFieldsFromExtra :: Extractor
extractFieldsFromExtra [] = error "The `extra` array doesn't have any `Fields` item"
extractFieldsFromExtra (s@ST.Fields{} : _) = s
extractFieldsFromExtra (_:ss) = extractFieldsFromExtra ss

extractCodeblockFromExtra :: Extractor
extractCodeblockFromExtra [] = error "The `extra` array doesn't have any `CodeBlock` item"
extractCodeblockFromExtra (s@ST.CodeBlock{} : _) = s
extractCodeblockFromExtra (_:ss) = extractCodeblockFromExtra ss

extractEmptyFunctionFromExtra :: Extractor
extractEmptyFunctionFromExtra [] = error "The `extra` array doesn't have any `EmptyFunction` item"
extractEmptyFunctionFromExtra (s@ST.EmptyFunction : _) = s
extractEmptyFunctionFromExtra (_:ss) = extractEmptyFunctionFromExtra ss

extractArgPositionFromExtra :: Extractor
extractArgPositionFromExtra [] = error "The `extra` array doesn't have any `ArgPosition` item"
extractArgPositionFromExtra (s@ST.ArgPosition{} : _) = s
extractArgPositionFromExtra (_:ss) = extractArgPositionFromExtra ss

extractOffsetFromExtra :: Extractor
extractOffsetFromExtra [] = error "The `extra` array doesn't have any `Offset` item"
extractOffsetFromExtra (s@ST.Offset{} : _) = s
extractOffsetFromExtra (_:ss) = extractOffsetFromExtra ss

runTestForInvalidProgram :: String -> IO ()
runTestForInvalidProgram program = do
    let ([], tokens) = L.scanTokens program
    RWS.runRWST (parse tokens) () ST.initialState `shouldThrow` anyException


type TestFunction a b
    = a
    -> ST.DictionaryEntry
    -> Extractor
    -> (ST.Extra -> Bool)
    -> IO b

testEntry :: TestFunction ST.Dictionary ()
testEntry dict expectedEntry extractor predicate = do
    let varName = ST.name expectedEntry
    let scope = ST.scope expectedEntry
    let chain = filter (\d -> ST.scope d == scope ) $ filter (\d -> ST.name d == varName) $ ST.findChain varName dict
    chain `shouldNotSatisfy` null
    let actualEntry = head chain
    ST.name actualEntry `shouldBe` varName
    ST.category actualEntry `shouldBe` ST.category expectedEntry
    ST.scope actualEntry `shouldBe` scope
    ST.entryType actualEntry `shouldBe` ST.entryType expectedEntry
    let extra' = ST.extra actualEntry
    extra' `shouldNotSatisfy` null
    extractor extra' `shouldSatisfy` predicate

test :: TestFunction String ST.Dictionary
test prog expectedEntry extractor predicate = do
    (_, ST.SymTable {ST.stDict=dict}, _) <- extractSymTable prog
    testEntry dict expectedEntry extractor predicate
    return dict

testVoid :: TestFunction String ()
testVoid prog expectedEntry extractor predicate = RWS.void $ test prog expectedEntry extractor predicate

shouldNotError :: String -> IO ()
shouldNotError p = do
    errors <- extractErrors p
    errors `shouldSatisfy` null

shouldErrorOn :: String -> (String, Int, Int) -> IO ()
shouldErrorOn program (_, row', col) = do
    errors <- extractErrors program
    errors `shouldNotSatisfy` null
    let Error _ posn = head errors
    col `shouldBe` column posn
    row' `shouldBe` row posn
