module FireLink.BackEnd.CodeGenerator where

import           Control.Monad.RWS              (RWST (..), get, put, tell)
import qualified FireLink.FrontEnd.Grammar      as G (Id (..))
import           FireLink.FrontEnd.SymTable     (Dictionary (..),
                                                 DictionaryEntry (..),
                                                 findChain, wordSize)
import           FireLink.FrontEnd.Tokens       (Token (..))
import           FireLink.FrontEnd.TypeChecking
import           TACType
import           Data.Map.Strict as Map

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

fall :: OperandType
fall = Label "-1"

isFall :: OperandType -> Bool
isFall (Label l) = l == "-1"
isFall _         = error "calling isFall with non-label"

genIdAssignment :: OperandType -> OperandType -> CodeGenMonad ()
genIdAssignment lValue rValue =
    gen [ThreeAddressCode
        { tacOperand = Assign
        , tacLvalue = Just lValue
        , tacRvalue1 = Just rValue
        , tacRvalue2 = Nothing
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
