module SymTable where

import qualified Control.Monad.RWS as RWS
import qualified Data.Map.Strict as Map
import Lexer (AlexPosn)

type Scope = Int
type ScopeStack = [Scope]


data SemanticError = SemanticError String AlexPosn
    deriving Show
type SemanticErrors = [SemanticError]

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
    deriving (Eq, Show)

data Extra = FuncParams DictionaryEntries
    | Size Int
    deriving Show

data DictionaryEntry = DictionaryEntry
    { name :: String
    , category :: Category
    , scope :: Scope
    , entryType :: Maybe DictionaryEntry
    , extra :: [Extra]
    } deriving Show

type DictionaryEntries = [DictionaryEntry]

type Dictionary = Map.Map String DictionaryEntries

type SymTable = (Dictionary, ScopeStack, Int)

type ParserMonad = RWS.RWST () SemanticErrors SymTable IO

findChain :: String -> Dictionary -> DictionaryEntries
findChain = Map.findWithDefault []

findPervasive :: String -> DictionaryEntries -> Maybe DictionaryEntry
findPervasive _ [] = Nothing
findPervasive s ds = case filter (\d -> name d == s) $ filter (\d -> scope d == 0) ds of
    [] -> Nothing
    a:_ -> Just a

findBest :: String -> DictionaryEntries -> ScopeStack -> Maybe DictionaryEntry
findBest _ _ [] = Nothing
findBest name' entries (s:ss) = case filter (\d -> scope d == s) entries of
    [] -> findBest name' entries ss
    [a] -> Just a
    s -> error $ "For some reason there was more than 1 \
    \symbol with the same name on the same scope" ++ show s

dictLookup :: String -> ParserMonad (Maybe DictionaryEntry)
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

addEntry :: DictionaryEntry -> ParserMonad ()
addEntry d@DictionaryEntry{name=n} = do
    (dict, st, cs) <- RWS.get
    let chains = findChain n dict
    RWS.put (Map.insert n (d:chains) dict, st, cs)

enterScope :: Scope -> ParserMonad ()
enterScope s = do
    (dict, st, cs) <- RWS.get
    RWS.put (dict, s:st, cs)

exitScope :: ParserMonad ()
exitScope = do
    (dict, st, cs) <- RWS.get
    case st of
        [] -> RWS.put (dict, [], cs)
        _:s -> RWS.put (dict, s, cs)

updateActualScope :: ParserMonad ()
updateActualScope = do
    (dict, st, cs) <- RWS.get
    RWS.put (dict, st, cs + 1)

initialState :: SymTable
initialState = (Map.fromList l, [0], 0)
    where l = [("int", [DictionaryEntry "int" Type 0 Nothing []])
            , ("bigInt", [DictionaryEntry "bigInt" Type 0 Nothing []])
            , ("float", [DictionaryEntry "float" Type 0 Nothing []])
            , ("char", [DictionaryEntry "char" Type 0 Nothing []])
            , ("3bool", [DictionaryEntry "3bool" Type 0 Nothing []])
            , ("array", [DictionaryEntry "array" Constructor 0 Nothing []])
            , ("string", [DictionaryEntry "string" Constructor 0 Nothing []])
            , ("set", [DictionaryEntry "set" Constructor 0 Nothing []])
            , ("pointer", [DictionaryEntry "pointer" Constructor 0 Nothing []])
            ]
