module SymTable where

import qualified Control.Monad.RWS as RWS
import qualified Data.Map.Strict as Map
import Grammar (Id)

type SemanticError = String
type Scope = Int
type Stack = [Scope]

data Category = Variable
    | Constant
    | Type
    | Procedure
    | Function
    | StructItem
    | EnumItem
    | UnionItem
    | RefParam
    | ValueParam
    | Constructor
    deriving Show

data Extra = FuncParams [DictionaryEntry]
    | Size Int

data DictionaryEntry = DictionaryEntry {
    name :: String,
    category :: Category,
    scope :: Scope,
    entryType :: Maybe DictionaryEntry,
    extra :: [Extra]
    }

type Dictionary = Map.Map String [DictionaryEntry]

type SymTable = (Dictionary, Stack, Int)

type ParserState = RWS.RWST () [SemanticError] SymTable IO
