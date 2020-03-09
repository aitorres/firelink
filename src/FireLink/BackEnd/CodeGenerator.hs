module FireLink.BackEnd.CodeGenerator where

import           Control.Monad.RWS              (RWST (..), get, put, tell)
import qualified FireLink.FrontEnd.Grammar      as G (Id (..))
import           FireLink.FrontEnd.SymTable     (Dictionary (..),
                                                 DictionaryEntry (..),
                                                 findChain)
import           FireLink.FrontEnd.Tokens       (Token (..))
import           FireLink.FrontEnd.TypeChecking
import           TACType

data CodeGenState = CodeGenState
    { cgsNextLabel :: !Int
    , cgsNextTemp :: !Int
    , cgsCurTempOffset :: !Int
    }

initialState :: CodeGenState
initialState = CodeGenState {cgsNextTemp = 0, cgsNextLabel = 0, cgsCurTempOffset = 0}

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

setTempOffset :: Int -> CodeGenMonad ()
setTempOffset offset = get >>= \state -> put state { cgsCurTempOffset = offset }

newLabel :: CodeGenMonad (Operand a b)
newLabel = do
    state@CodeGenState {cgsNextLabel = label} <- get
    put $ state{cgsNextLabel = label + 1}
    return $ Label $ show label

genGoTo :: OperandType -> CodeGenMonad ()
genGoTo label = tell [ThreeAddressCode
            { tacOperand = GoTo
            , tacLvalue = Nothing
            , tacRvalue1 = Nothing
            , tacRvalue2 = Just label
            }]

genLabel :: OperandType -> CodeGenMonad ()
genLabel label = tell [ThreeAddressCode
                            { tacOperand = NewLabel
                            , tacLvalue = Nothing
                            , tacRvalue1 = Just label
                            , tacRvalue2 = Nothing
                            }]

fall :: OperandType
fall = Label "-1"

isFall :: OperandType -> Bool
isFall (Label l) = l == "-1"
isFall _         = error "calling isFall with non-label"

genIdAssignment :: OperandType -> OperandType -> CodeGenMonad ()
genIdAssignment lValue rValue =
    tell [ThreeAddressCode
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
    tell [
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
