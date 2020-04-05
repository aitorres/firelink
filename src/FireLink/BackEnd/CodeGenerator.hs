module FireLink.BackEnd.CodeGenerator where

import           Control.Monad.RWS              (RWST (..), ask, get, put, tell)
import           Data.Map.Strict                as Map
import qualified FireLink.FrontEnd.Grammar      as G (Id (..))
import           FireLink.FrontEnd.SymTable     (Dictionary (..),
                                                 DictionaryEntry (..),
                                                 Extra (..), findChain,
                                                 findSymEntryByName, findWidth,
                                                 wordSize)
import           FireLink.FrontEnd.Tokens       (Token (..))
import           FireLink.FrontEnd.TypeChecking
import           TACType

data CodeGenState = CodeGenState
    { cgsNextLabel :: !Int
    , cgsNextTemp :: !Int
    , cgsCurTempOffset :: !Int
    , cgsArrayOffsetVar :: !TACSymEntry -- This variable holds the current offset for arrays
    , cgsTemporalsToReplace :: ![(TACSymEntry, Int)]

    -- ^ To save array intermediate sizes we associate the _alias generated
    -- with a TACSymEntry that contains its size
    , cgsArrayWidthMap :: !(Map.Map String OperandType)
    }
    deriving Show

initialState :: CodeGenState
initialState = CodeGenState
    { cgsNextTemp = 0
    , cgsNextLabel = 0
    , cgsCurTempOffset = 0
    , cgsArrayOffsetVar = TACTemporal "" (-1)
    , cgsTemporalsToReplace = []
    , cgsArrayWidthMap = Map.empty
    }

type Offset = Int

data TACSymEntry
    = TACTemporal String Offset
    | TACVariable DictionaryEntry Offset
    deriving Eq

getTACSymEntryOffset :: TACSymEntry -> Int
getTACSymEntryOffset (TACTemporal _ o) = o
getTACSymEntryOffset (TACVariable _ o) = o

instance SymEntryCompatible TACSymEntry where
    getSymID (TACTemporal s o)          = "(" ++ s ++ ")base[" ++ show o ++ "]"
    getSymID (TACVariable entry offset) = "(" ++ name entry ++ ")base[" ++ show offset ++ "]"

instance Show TACSymEntry where
    show = getSymID

type TAC = ThreeAddressCode TACSymEntry Type
type OperandType = Operand TACSymEntry Type

type CodeGenMonad = RWST Dictionary [TAC] CodeGenState IO

newtemp :: CodeGenMonad TACSymEntry
newtemp = do
    state@CodeGenState {cgsNextTemp = temp, cgsCurTempOffset = offset } <- get
    put $ state{cgsNextTemp = temp + 1, cgsCurTempOffset = offset + 4}
    return $ TACTemporal ("_t" ++ show temp) offset

putArrayOffsetVar :: TACSymEntry -> CodeGenMonad ()
putArrayOffsetVar e = get >>= \state -> put state { cgsArrayOffsetVar = e }

getArrayOffsetVar :: CodeGenMonad TACSymEntry
getArrayOffsetVar = cgsArrayOffsetVar <$> get

setTempOffset :: Int -> CodeGenMonad ()
setTempOffset offset = get >>= \state -> put state { cgsCurTempOffset = offset }

getTempOffset :: CodeGenMonad Int
getTempOffset = cgsCurTempOffset <$> get

getTemporalsToReplace :: CodeGenMonad [(TACSymEntry, Int)]
getTemporalsToReplace = cgsTemporalsToReplace <$> get

addTemp :: TACSymEntry -> Int -> CodeGenMonad ()
addTemp entry n = do
    state@CodeGenState {cgsTemporalsToReplace = temps} <- get
    put state{cgsTemporalsToReplace = (entry, n) : temps}

newLabel :: CodeGenMonad (Operand a b)
newLabel = do
    state@CodeGenState {cgsNextLabel = label} <- get
    put $ state{cgsNextLabel = label + 1}
    return $ Label $ show label

genGoTo :: OperandType -> CodeGenMonad ()
genGoTo label = gen [ThreeAddressCode
            { tacOperand = GoTo
            , tacLvalue = Nothing
            , tacRvalue1 = Nothing
            , tacRvalue2 = Just label
            }]

genLabel :: OperandType -> CodeGenMonad ()
genLabel label = gen [ThreeAddressCode
                            { tacOperand = NewLabel
                            , tacLvalue = Nothing
                            , tacRvalue1 = Just label
                            , tacRvalue2 = Nothing
                            }]

putArrayEntrySize :: String -> OperandType -> CodeGenMonad ()
putArrayEntrySize a o = do
    state@CodeGenState{cgsArrayWidthMap=m} <- get
    put state{cgsArrayWidthMap = Map.insert a o m}

getArrayMap :: CodeGenMonad (Map.Map String OperandType)
getArrayMap = cgsArrayWidthMap <$> get

lookupArrayMap :: String -> CodeGenMonad (Maybe OperandType)
lookupArrayMap s = Map.lookup s <$> getArrayMap

fall :: OperandType
fall = Label "-1"

isFall :: OperandType -> Bool
isFall (Label l) = l == "-1"
isFall _         = error "calling isFall with non-label"

isProgramEnd :: Operation -> Bool
isProgramEnd = flip elem [Exit, Abort]

isUnconditionalJump :: Operation -> Bool
isUnconditionalJump = flip elem [GoTo, Call, Return]

isConditionalJump :: Operation -> Bool
isConditionalJump = flip elem [If, Eq, Neq, Gt, Lt, Gte, Lte]

isJump :: Operation -> Bool
isJump op = isUnconditionalJump op || isConditionalJump op || isProgramEnd op

genIdAssignment :: OperandType -> OperandType -> CodeGenMonad ()
genIdAssignment lValue rValue =
    gen [ThreeAddressCode
        { tacOperand = Assign
        , tacLvalue = Just lValue
        , tacRvalue1 = Just rValue
        , tacRvalue2 = Nothing
        }]

genSetAssignment :: OperandType -> OperandType -> OperandType -> CodeGenMonad ()
genSetAssignment base offset value =
    gen [ThreeAddressCode
        { tacOperand = Set
        , tacLvalue = Just base
        , tacRvalue1 = Just offset
        , tacRvalue2 = Just value
        }]

class GenerateCode a where
    genCode :: a -> CodeGenMonad ()

raiseRunTimeError :: String -> CodeGenMonad ()
raiseRunTimeError msg = do
    let msgOperand = Constant (msg, StringT)
    gen [
        ThreeAddressCode
        { tacOperand = Print
        , tacLvalue = Nothing
        , tacRvalue1 = Just msgOperand
        , tacRvalue2 = Nothing
        },
        ThreeAddressCode
        { tacOperand = Abort
        , tacLvalue = Nothing
        , tacRvalue1 = Nothing
        , tacRvalue2 = Nothing
        }]

gen :: [TAC] -> CodeGenMonad ()
gen = tell

alignedOffset :: Int -> Int
alignedOffset maxOffset =
    if maxOffset `mod` wordSize == 0
    then maxOffset
    else maxOffset +  wordSize - (maxOffset `mod` wordSize)

typeWidth :: String -> CodeGenMonad OperandType
typeWidth t = do
    resultLookup <- lookupArrayMap t
    case resultLookup of
        -- It is an array and its size is known at compile time
        Just o -> return o

        -- The width is on symtable
        _ -> do
            t' <- findSymEntryByName t <$> ask
            let (Width w) = findWidth t'
            let w' = alignedOffset w
            return $ Constant (show w', BigIntT)

-- | Auxiliary function that determines if a given OperandType
-- | is an Id (as opposed to Labels or Constants)
isId :: OperandType -> Bool
isId (Id _) = True
isId _      = False
