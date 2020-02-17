module Utils where

import qualified Control.Monad.RWS          as RWS
import           FireLink.FrontEnd.Grammar
import           FireLink.FrontEnd.Lexer    (scanTokens)
import           FireLink.FrontEnd.Parser
import qualified FireLink.FrontEnd.SymTable as ST
import qualified Test.Hspec                 as TH

runTestForValidProgram :: String -> (Program -> Bool) -> IO ()
runTestForValidProgram program predicate = do
    let ([], tokens) = scanTokens program
    (ast, _, _) <- RWS.runRWST (parse tokens) () ST.initialState
    ast `TH.shouldSatisfy` predicate

runTestForInvalidProgram :: String -> IO ()
runTestForInvalidProgram program = do
    let ([], tokens) = scanTokens program
    RWS.runRWST (parse tokens) () ST.initialState `TH.shouldThrow` TH.anyException
