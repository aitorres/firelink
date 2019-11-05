module SymTable where

import qualified Control.Monad.RWS as RWS
import qualified Data.Map.Strict as Map
import qualified Lexer as L
import qualified Grammar as G

type Scope = Int
type ScopeStack = [Scope]


data SemanticError = SemanticError String L.Token
    deriving Show
type SemanticErrors = [SemanticError]

data Category = Variable
    | Constant
    | Type
    | Procedure
    | Function
    | RecordItem
    | EnumItem
    | UnionItem
    | RefParam
    | ValueParam
    | Constructor
    deriving (Eq, Show)



data Extra
    = Recursive -- For sets alike
        String -- (pointer to) constructor
        Extra -- Type perse

    | Compound -- For strings alike
        String -- (pointer to) Constructor
        G.Expr -- Size

    | CompoundRec -- For arrays alike
        String -- (pointer to) constructor
        G.Expr -- Size
        Extra -- Type perse

    | Fields -- For functions or names that need to refer to another scope
             -- in order to construct itself
        Scope -- We only need the scope where the fields live

    | EmptyFunction -- For functions/procs that doesn't have any arguments

    | CodeBlock -- For names that carries a codeblock ast
        G.CodeBlock

    | Simple String -- For non-composite types

    | ArgPosition Int -- For argument list position
    deriving Show

data DictionaryEntry = DictionaryEntry
    { name :: !String -- ^ Entry name
    , category :: !Category -- ^ Category of the entry
    , scope :: !Scope -- ^ Current scope at entry insertion
    , entryType :: !(Maybe String) -- (pointer to another) entry
    , extra :: ![Extra] -- extra data semantic to the real dictionary
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
        je@(Just _) -> je
        Nothing -> case pervasive of
            je@(Just _) -> je
            _ -> Nothing)

addEntry :: DictionaryEntry -> ParserMonad ()
addEntry d@DictionaryEntry{name=n} = do
    (dict, st, cs) <- RWS.get
    let chains = findChain n dict
    RWS.put (Map.insert n (d:chains) dict, st, cs)

updateEntry :: (DictionaryEntries -> Maybe DictionaryEntries) -> String -> ParserMonad ()
updateEntry f s = do
    (dict, st, cs) <- RWS.get
    RWS.put (Map.update f s dict, st, cs)

enterScope :: ParserMonad ()
enterScope = do
    (dict, st, cs) <- RWS.get
    let cs' = cs + 1
    RWS.put (dict, cs':st, cs')

exitScope :: ParserMonad ()
exitScope = do
    (dict, st, cs) <- RWS.get
    case st of
        [] -> RWS.put (dict, [], cs)
        _:s -> RWS.put (dict, s, cs)

smallHumanity :: String
smallHumanity = "small humanity"
humanity :: String
humanity = "humanity"
hollow :: String
hollow = "hollow"
sign :: String
sign = "sign"
bonfire :: String
bonfire = "bonfire"
chest :: String
chest = ">-chest"
miracle :: String
miracle = ">-miracle"
armor :: String
armor = "armor"
arrowTo :: String
arrowTo = "arrow to"
bezel :: String
bezel = "bezel"
link :: String
link = "link"
void :: String
void = "void"

initialState :: SymTable
initialState = (Map.fromList l, [0], 0)
    where l = [(smallHumanity, [DictionaryEntry smallHumanity Type 0 Nothing []])
            , (humanity, [DictionaryEntry humanity Type 0 Nothing []])
            , (hollow, [DictionaryEntry hollow Type 0 Nothing []])
            , (sign, [DictionaryEntry sign Type 0 Nothing []])
            , (bonfire, [DictionaryEntry bonfire Type 0 Nothing []])
            , (chest, [DictionaryEntry chest Constructor 0 Nothing []])
            , (miracle, [DictionaryEntry miracle Constructor 0 Nothing []])
            , (armor, [DictionaryEntry armor Constructor 0 Nothing []])
            , (arrowTo, [DictionaryEntry arrowTo Constructor 0 Nothing []])
            , (bezel, [DictionaryEntry bezel Constructor 0 Nothing []])
            , (link, [DictionaryEntry link Constructor 0 Nothing []])
            , (void, [DictionaryEntry void Type 0 Nothing []])
            ]

tokensToEntryName :: L.Token -> String
tokensToEntryName (L.Token at _ _) = case at of
    L.TkBigInt -> humanity
    L.TkSmallInt -> smallHumanity
    L.TkFloat -> hollow
    L.TkChar -> sign
    L.TkBool -> bonfire
    L.TkString -> miracle
    L.TkArray -> chest
    L.TkSet -> armor
    L.TkRecord -> bezel
    L.TkUnionStruct -> link
    a -> error $ "Token " ++ show a ++ "doesn't map to anything"
