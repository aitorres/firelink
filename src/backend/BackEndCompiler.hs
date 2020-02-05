module BackEndCompiler where

import Grammar (Program(..))
import SymTable (Dictionary(..))
import CodeGenerator (initialState, TAC(..), genCode)
import InstructionCodeGenerator ()
import Control.Monad.RWS (runRWST)

backend :: Program -> Dictionary -> IO [TAC]
backend program dictionary = do
    (_, _, code) <- runRWST (genCode program) dictionary initialState
    return code
