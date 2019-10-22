module SymTable where

import qualified Control.Monad.RWS as RWS
import qualified Data.Map.Strict as Map

type SemanticError = String
type Scope = Int
type Stack = [Scope]

newtype DictionaryEntry = DictionaryEntry String

type Dictionary = Map.Map String DictionaryEntry


type SymTable = (Dictionary, Stack)

type ParserState = RWS.RWST () [SemanticError] SymTable IO
