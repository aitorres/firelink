module CodeGenerator where

import Control.Monad.RWS (RWST(..), get, put)
import TACType
import TypeChecking
import SymTable (DictionaryEntry(..), Dictionary(..))

newtype CodeGenState = CodeGenState { cgsNextLabel :: Int }

initialState :: CodeGenState
initialState = CodeGenState {cgsNextLabel = 0}

data TACSymEntry = TACTemporal String | TACVariable DictionaryEntry

instance SymEntryCompatible TACSymEntry where
    getSymID (TACTemporal s) = s
    getSymID (TACVariable entry) = name entry

instance Show TACSymEntry where
    show = getSymID

type TAC = ThreeAddressCode TACSymEntry Type

type CodeGenMonad = RWST Dictionary [TAC] CodeGenState IO

newtemp :: CodeGenMonad TACSymEntry
newtemp = do
    state@CodeGenState {cgsNextLabel = label} <- get
    put $ state{cgsNextLabel = label + 1}
    return $ TACTemporal $ "_t" ++ show label

class GenerateCode a where
    genCode :: a -> CodeGenMonad ()
