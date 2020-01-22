module BackEndCompiler where

import Grammar (Program(..))
import CodeGenerator (initialState, TAC(..), genCode)
import ProgramCodeGenerator ()
import Control.Monad.RWS (runRWST)

backend :: Program -> IO [TAC]
backend program = do
    (_, _, code) <- runRWST (genCode program) () initialState
    return code
