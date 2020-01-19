module Utils where

import qualified SymTable as ST
import Lexer (scanTokens)
import qualified Test.Hspec as TH
import Parser
import Grammar
import qualified Control.Monad.RWS as RWS

runTestForValidProgram :: String -> (Program -> Bool) -> IO ()
runTestForValidProgram program predicate = do
    let ([], tokens) = scanTokens program
    (ast, _, _) <- RWS.runRWST (parse tokens) () ST.initialState
    ast `TH.shouldSatisfy` predicate

runTestForInvalidProgram :: String -> IO ()
runTestForInvalidProgram program = do
    let ([], tokens) = scanTokens program
    RWS.runRWST (parse tokens) () ST.initialState `TH.shouldThrow` TH.anyException
