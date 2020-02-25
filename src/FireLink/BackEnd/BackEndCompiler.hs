module FireLink.BackEnd.BackEndCompiler (
    backend
) where

import           Control.Monad.RWS                         (runRWST)
import           FireLink.BackEnd.CodeGenerator            (TAC (..), genCode,
                                                            initialState)
import           FireLink.BackEnd.InstructionCodeGenerator ()
import           FireLink.BackEnd.Optimizer                (optimize)
import           FireLink.FrontEnd.Grammar                 (Program (..))
import           FireLink.FrontEnd.SymTable                (Dictionary (..))
import           TACType

backend :: Program -> Dictionary -> IO [TAC]
backend program dictionary = do
    (_, _, code) <- runRWST (genCode program) dictionary initialState
    return $ optimize code
