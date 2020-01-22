module CodeGenerator where

import Control.Monad.RWS (RWST(..), get, put)
import TACType
import SymTable (DictionaryEntry(..))

instance SymEntryCompatible DictionaryEntry where
    getSymID = name

newtype CodeGenState = CodeGenState { cgsNextLabel :: Int }

initialState :: CodeGenState
initialState = CodeGenState {cgsNextLabel = 0}

type TAC = ThreeAddressCode DictionaryEntry ConstantValue

data ConstantValue = ConstantInt Int | ConstantString String

type CodeGenMonad = RWST () [TAC] CodeGenState IO

newtemp :: CodeGenMonad String
newtemp = do
    state@CodeGenState {cgsNextLabel = label} <- get
    put $ state{cgsNextLabel = label + 1}
    return $ "t" ++ show label

class GenerateCode a where
    genCode :: a -> CodeGenMonad ()

