module SymTable where

import qualified Control.Monad.RWS as RWS
import qualified Data.Map.Strict as Map
import qualified Tokens as T
import qualified Grammar as G
import Errors

import Data.Sort (sortBy)

type Scope = Int
type ScopeStack = [Scope]

data Category
    = Variable
    | Constant
    | Type
    | Procedure
    | Function
    | RecordItem
    | UnionItem
    | RefParam
    | ValueParam
    | Constructor
    deriving (Eq, Show)


data TypeFields = Callable | Union | Record
    deriving (Show, Eq)

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

    | Fields
        TypeFields
        Scope -- We only need the scope where the fields live

    | EmptyFunction -- For functions/procs that doesn't have any arguments

    | CodeBlock -- For names that carries a codeblock ast
        G.CodeBlock

    | Simple String -- For non-composite types

    | ArgPosition Int -- For argument list position
    deriving Show

isFieldsExtra :: Extra -> Bool
isFieldsExtra (Fields _ _) = True
isFieldsExtra _ = False

isEmptyFunction :: Extra -> Bool
isEmptyFunction EmptyFunction = True
isEmptyFunction _ = False

isArgPosition :: Extra -> Bool
isArgPosition ArgPosition{} = True
isArgPosition _ = False

findArgPosition :: [Extra] -> Extra
findArgPosition = head . filter isArgPosition

findFieldsExtra :: Extra -> Maybe Extra
findFieldsExtra a@Fields{} = Just a
findFieldsExtra (CompoundRec _ _ e) = findFieldsExtra e
findFieldsExtra (Recursive _ e) = findFieldsExtra e
findFieldsExtra _ = Nothing

isExtraAType :: Extra -> Bool
isExtraAType Recursive{} = True
isExtraAType Compound{} = True
isExtraAType CompoundRec{} = True
isExtraAType Fields{} = True
isExtraAType Simple{} = True
isExtraAType _ = False

extractTypeFromExtra :: [Extra] -> Extra
extractTypeFromExtra = head . filter isExtraAType

{-|
    Dictionary entries represent "names" in the programming languages. With
    "names" we refer anything that has a name and a value. There are some
    implicit constraints that each name implies to the contents of `extra`,
    depending on its `category`:
-}
data DictionaryEntry = DictionaryEntry
    { name :: !String -- ^ Entry name
    , category :: !Category -- ^ Category of the entry
    , scope :: !Scope -- ^ Current scope at entry insertion
    , entryType :: !(Maybe String) -- (pointer to another) entry
    , extra :: ![Extra] -- extra data semantic to the real dictionary
    } deriving Show

type DictionaryEntries = [DictionaryEntry]

type Dictionary = Map.Map String DictionaryEntries

data SymTable = SymTable
    { stDict :: !Dictionary, -- ^ Dictionary of valid symbols
      stScopeStack :: !ScopeStack, -- ^ Stack of scopes at the current state
      stCurrScope :: !Int, -- ^ Current scope
      stIterationVars :: ![String], -- ^ List of currently protected iteration variables
      stIterableVars :: ![String], -- ^ List of currently protected iterable variable
      stVisitedMethod :: !(Maybe String) -- ^ List of currently visited method
    }

type ParserMonad = RWS.RWST () [Error] SymTable IO

findAllInScope :: Scope -> Dictionary -> DictionaryEntries
findAllInScope s dict = filter (\entry -> scope entry == s) $ concatMap snd $ Map.toList dict

sortByArgPosition :: DictionaryEntries -> DictionaryEntries
sortByArgPosition = sortBy sortFun
    where
        sortFun :: DictionaryEntry -> DictionaryEntry -> Ordering
        sortFun d d' =
            let (ArgPosition i) = findArgPosition $ extra d in
            let (ArgPosition j) = findArgPosition $ extra d' in
                i `compare` j

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
    e -> error $ "For some reason there was more than 1 \
    \symbol with the same name on the same scope" ++ show e

dictLookup :: String -> ParserMonad (Maybe DictionaryEntry)
dictLookup n = do
    SymTable {stDict=dict, stScopeStack=stack} <- RWS.get
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
    st@SymTable {stDict=dict} <- RWS.get
    let chains = findChain n dict
    RWS.put st{stDict=Map.insert n (d:chains) dict}

updateEntry :: (DictionaryEntries -> Maybe DictionaryEntries) -> String -> ParserMonad ()
updateEntry f s = do
    st@SymTable {stDict=dict} <- RWS.get
    RWS.put st{stDict=Map.update f s dict}

enterScope :: ParserMonad ()
enterScope = do
    st@SymTable {stCurrScope=cs, stScopeStack=scopeStack} <- RWS.get
    RWS.put st{stCurrScope=cs + 1, stScopeStack=cs + 1 : scopeStack}

exitScope :: ParserMonad ()
exitScope = do
    st@SymTable {stScopeStack=scopeStack} <- RWS.get
    case scopeStack of
        [] -> RWS.put st{stScopeStack=[]}
        _:s -> RWS.put st{stScopeStack=s}

addIteratorVariable :: String -> ParserMonad ()
addIteratorVariable var = do
    st@SymTable {stIterationVars=iterationVars} <- RWS.get
    RWS.put st{stIterationVars=var : iterationVars}

popIteratorVariable :: ParserMonad ()
popIteratorVariable = do
    st@SymTable {stIterationVars=iterationVars} <- RWS.get
    RWS.put st{stIterationVars=case iterationVars of
        [] -> []
        _:s -> s}

addVisitedMethod :: String -> ParserMonad ()
addVisitedMethod method = do
    st <- RWS.get
    RWS.put st{stVisitedMethod = Just method}

popVisitedMethod :: ParserMonad ()
popVisitedMethod  = do
    st <- RWS.get
    RWS.put st{stVisitedMethod = Nothing}

addIterableVariable :: String -> ParserMonad ()
addIterableVariable var = do
    st@SymTable {stIterableVars=iterableVars} <- RWS.get
    RWS.put st{stIterableVars= var: iterableVars}

popIterableVariable :: ParserMonad ()
popIterableVariable = do
    st@SymTable {stIterableVars=iterableVars} <- RWS.get
    RWS.put st{stIterableVars=case iterableVars of
        [] -> []
        _:s -> s}

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
errorType :: String
errorType = "errorType"
arrow :: String
arrow = "arrow"

initialState :: SymTable
initialState = SymTable (Map.fromList l) [1, 0] 1 [] [] Nothing
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
            , (arrow, [DictionaryEntry arrow Constructor 0 Nothing []])
            , (void, [DictionaryEntry void Type 0 Nothing []])
            ]

tokensToEntryName :: T.Token -> String
tokensToEntryName tk@T.Token {T.aToken=at, T.cleanedString=s} = case at of
    T.TkBigInt -> humanity
    T.TkSmallInt -> smallHumanity
    T.TkFloat -> hollow
    T.TkChar -> sign
    T.TkBool -> bonfire
    T.TkString -> miracle
    T.TkArray -> chest
    T.TkSet -> armor
    T.TkRecord -> bezel
    T.TkUnionStruct -> link
    T.TkId -> s
    T.TkPointer -> arrow
    _ -> error $ "Token " ++ show tk ++ " doesn't map to anything"
