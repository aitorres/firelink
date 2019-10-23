module SymTable where

import qualified Control.Monad.RWS as RWS
import qualified Data.Map.Strict as Map
import Grammar (Id)

type SemanticError = String
type Scope = Int
type ScopeStack = [Scope]

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
    deriving Show

data DictionaryEntry = DictionaryEntry {
    name :: String,
    category :: Category,
    scope :: Scope,
    entryType :: Maybe DictionaryEntry,
    extra :: [Extra]
    }
    deriving Show

type DictionaryEntries = [DictionaryEntry]

type Dictionary = Map.Map String DictionaryEntries

type SymTable = (Dictionary, ScopeStack, Int)

type ParserState = RWS.RWST () [SemanticError] SymTable IO

findChain :: String -> Dictionary -> DictionaryEntries
findChain = Map.findWithDefault []

findPervasive :: String -> DictionaryEntries -> Maybe DictionaryEntry
findPervasive _ [] = Nothing
findPervasive s ds = case filter (\d -> scope d == 0) ds of
    [] -> Nothing
    a:_ -> Just a

findBest :: String -> DictionaryEntries -> ScopeStack -> Maybe DictionaryEntry
findBest _ _ [] = Nothing
findBest name entries (s:ss) = case filter (\d -> scope d == s) entries of
    [] -> findBest name entries ss
    [a] -> Just a
    s -> error $ "For some reason there was more than 1 symbol with the same name on the same scope" ++ show s

dictLookup :: String -> ParserState (Maybe DictionaryEntry)
dictLookup n = do
    (dict, stack, _) <- RWS.get
    let chain = filter (\d -> name d == n) $ findChain n dict
    let pervasive = findPervasive n chain
    let best = findBest n chain stack
    return (case best of
        je@(Just entry) -> je
        Nothing -> case pervasive of
            je@(Just entry) -> je
            _ -> Nothing)
