module SymTable where

import qualified Control.Monad.RWS as RWS
import qualified Data.Map.Strict as Map
import Grammar (Id)


type StructItem = (Id, Type)
type StructItems = [StructItem]

data Type
    = BigInt
    | SmallInt
    | FloatT
    | CharT
    | BoolT
    | StringType
    | Array Type
    | Set DictionaryEntry
    | Enum [DictionaryEntry]
    | Record [DictionaryEntry]
    | UnionStruct [DictionaryEntry]
    | Pointer [DictionaryEntry]
    deriving Show

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
    deriving Show


data DictionaryEntry = DictionaryEntry {
    name :: EntryName,
    category :: Category,
    scope :: Int,
    entryType :: Maybe DictionaryEntry,
    extra :: []
    }

type Dictionary = Map.Map String [DictionaryEntry]

type SymTable = (Dictionary, Stack, Int)

type ParserState = RWS.RWST () [SemanticError] SymTable IO
