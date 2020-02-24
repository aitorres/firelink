module FireLink.FrontEnd.SymTable where

import qualified Control.Monad.RWS              as RWS
import qualified Data.Map.Strict                as Map
import           FireLink.FrontEnd.Errors
import qualified FireLink.FrontEnd.Grammar      as G
import qualified FireLink.FrontEnd.Tokens       as T
import qualified FireLink.FrontEnd.TypeChecking as TC

import           Data.Sort                      (sortBy)

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

    | CodeBlock -- For names that carries a codeblock ast
        G.CodeBlock

    | Simple String -- For non-composite types

    | ArgPosition Int -- For argument list position

    -- Offset to retrieve variable at TAC. Only used in variables/constants
    | Offset Int

    -- Width of type. It is not world-aligned
    | Width Int

    -- Unique identifier for each union-attribute
    | UnionAttrId Int
    deriving Show

isWidthExtra :: Extra -> Bool
isWidthExtra Width{} = True
isWidthExtra _ = False

isUnionAttrId :: Extra -> Bool
isUnionAttrId UnionAttrId{} = True
isUnionAttrId _ = False

isFieldsExtra :: Extra -> Bool
isFieldsExtra (Fields _ _) = True
isFieldsExtra _            = False

isArgPosition :: Extra -> Bool
isArgPosition ArgPosition{} = True
isArgPosition _             = False

findArgPosition :: [Extra] -> Extra
findArgPosition extras = let filtered = filter isArgPosition extras in
    if null filtered then error "findArgPosition didn't found an ArgPosition"
    else head filtered


findWidth :: DictionaryEntry -> Extra
findWidth entry = f $ extra entry
    where
        f :: [Extra] -> Extra
        f extras = let widthExtras = filter isWidthExtra extras in
            if null widthExtras then error $ "Width extra not found for entry " ++ name entry
            else head widthExtras

findFieldsExtra :: Extra -> Maybe Extra
findFieldsExtra a@Fields{}          = Just a
findFieldsExtra (CompoundRec _ _ e) = findFieldsExtra e
findFieldsExtra (Recursive _ e)     = findFieldsExtra e
findFieldsExtra _                   = Nothing

isExtraAType :: Extra -> Bool
isExtraAType Recursive{}   = True
isExtraAType Compound{}    = True
isExtraAType CompoundRec{} = True
isExtraAType (Fields Record _) = True
isExtraAType (Fields Union _) = True
isExtraAType Simple{}      = True
isExtraAType _             = False

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
      stIterableVars :: ![String], -- ^ List of currently protected iterable variables
      stSwitchTypes :: ![TC.Type], -- ^ List of currently active switch variable types
      stVisitedMethod :: !(Maybe String), -- ^ List of currently visited method
      stNextAnonymousAlias :: !Int, -- ^ Next anonymous alias to be used with anonymous data types
      stNextParamId :: !Int, -- ^ Used to generate next parameter id (for ordering) in functions
      stOffsetStack :: ![Int], -- ^ Used to carry offset in scopes
      stUnionAttrIdStack :: ![Int] -- ^ Used to assign an unique identifier to each union-attr.
                                   -- we need a stack because we can have nested unions
    }
    deriving Show

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
    []  -> Nothing
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
            _           -> Nothing)

addEntry :: DictionaryEntry -> ParserMonad ()
addEntry d@DictionaryEntry{name=n} = do
    st@SymTable {stDict=dict} <- RWS.get
    let chains = findChain n dict
    RWS.put st{stDict=Map.insert n (d:chains) dict}

updateEntry :: (DictionaryEntries -> Maybe DictionaryEntries) -> String -> ParserMonad ()
updateEntry f s = do
    st@SymTable {stDict=dict} <- RWS.get
    RWS.put st{stDict=Map.update f s dict}

genAliasName :: ParserMonad String
genAliasName = do
    st@SymTable {stNextAnonymousAlias = nextAliasId} <- RWS.get
    RWS.put st{stNextAnonymousAlias = nextAliasId + 1}
    return $ "_alias_" ++ show nextAliasId

genNextArgPosition :: ParserMonad Int
genNextArgPosition = do
    st@SymTable {stNextParamId = nextParamId} <- RWS.get
    RWS.put st{stNextParamId = nextParamId + 1}
    return nextParamId

resetArgPosition :: ParserMonad ()
resetArgPosition = do
    st <- RWS.get
    RWS.put st{stNextParamId = 0}

enterScope :: ParserMonad ()
enterScope = do
    st@SymTable {stCurrScope=cs, stScopeStack=scopeStack} <- RWS.get
    RWS.put st{stCurrScope=cs + 1, stScopeStack=cs + 1 : scopeStack}

pushScope :: Int ->ParserMonad ()
pushScope c = do
    st@SymTable {stScopeStack=scopeStack} <- RWS.get
    RWS.put st{stScopeStack=c : scopeStack}

exitScope :: ParserMonad ()
exitScope = do
    st@SymTable {stScopeStack=scopeStack} <- RWS.get
    case scopeStack of
        []  -> RWS.put st{stScopeStack=[]}
        _:s -> RWS.put st{stScopeStack=s}

getCurrentScope :: ParserMonad Scope
getCurrentScope = do
    st@SymTable {stScopeStack=(currScope : _)} <- RWS.get
    return currScope

addIteratorVariable :: String -> ParserMonad ()
addIteratorVariable var = do
    st@SymTable {stIterationVars=iterationVars} <- RWS.get
    RWS.put st{stIterationVars=var : iterationVars}

popIteratorVariable :: ParserMonad ()
popIteratorVariable = do
    st@SymTable {stIterationVars=iterationVars} <- RWS.get
    RWS.put st{stIterationVars=case iterationVars of
        []  -> []
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
        []  -> []
        _:s -> s}

addSwitchType :: TC.Type -> ParserMonad ()
addSwitchType tp = do
    st@SymTable {stSwitchTypes=types} <- RWS.get
    RWS.put st{stSwitchTypes= tp:types}

popSwitchType :: ParserMonad ()
popSwitchType = do
    st@SymTable {stSwitchTypes=types} <- RWS.get
    RWS.put st{stSwitchTypes=case types of
        []  -> []
        _:s -> s}

getNextOffset :: ParserMonad Int
getNextOffset = do
    SymTable {stOffsetStack=offset : _} <- RWS.get
    return offset

putNextOffset :: Int -> ParserMonad ()
putNextOffset offset = do
    st@SymTable{stOffsetStack = _ : offsets} <- RWS.get
    RWS.put st{stOffsetStack = offset : offsets}

pushOffset :: Int -> ParserMonad ()
pushOffset offset = do
    st@SymTable{stOffsetStack = offsets} <- RWS.get
    RWS.put st{stOffsetStack = offset : offsets}

popOffset :: ParserMonad ()
popOffset = do
    st@SymTable{stOffsetStack = _ : offsets} <- RWS.get
    RWS.put st{stOffsetStack = offsets}

newUnion :: ParserMonad ()
newUnion = do
    st@SymTable{stUnionAttrIdStack = unions} <- RWS.get
    RWS.put st{stUnionAttrIdStack = 0 : unions}

genNextUnion :: ParserMonad Int
genNextUnion = do
    st@SymTable{stUnionAttrIdStack = union : unions} <- RWS.get
    RWS.put st{stUnionAttrIdStack = (union + 1) : unions}
    return union

popUnion :: ParserMonad ()
popUnion = do
    st@SymTable{stUnionAttrIdStack = _ : unions} <- RWS.get
    RWS.put st{stUnionAttrIdStack = unions}

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
arrow :: String
arrow = "arrow to"
bezel :: String
bezel = "bezel"
link :: String
link = "link"
void :: String
void = "void"
errorType :: String
errorType = "_errorType"

wordSize :: Int
wordSize = 4

initialState :: SymTable
initialState = SymTable (Map.fromList l) [1, 0] 1 [] [] [] Nothing 0 0 [] []
    where l = [(smallHumanity, [DictionaryEntry smallHumanity Type 0 Nothing [Width 2]])
            , (humanity, [DictionaryEntry humanity Type 0 Nothing [Width 4]])
            , (hollow, [DictionaryEntry hollow Type 0 Nothing [Width 8]])
            , (sign, [DictionaryEntry sign Type 0 Nothing [Width 1]])
            , (bonfire, [DictionaryEntry bonfire Type 0 Nothing [Width 1]])
            , (chest, [DictionaryEntry chest Constructor 0 Nothing []])
            , (miracle, [DictionaryEntry miracle Constructor 0 Nothing []])
            , (armor, [DictionaryEntry armor Constructor 0 Nothing []])
            , (bezel, [DictionaryEntry bezel Constructor 0 Nothing []])
            , (link, [DictionaryEntry link Constructor 0 Nothing []])
            , (arrow, [DictionaryEntry arrow Constructor 0 Nothing []])
            , (void, [DictionaryEntry void Type 0 Nothing []])
            ]

preparsedState :: SymTable -> SymTable
preparsedState SymTable { stDict=predict } = initialState { stDict=predict }

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
