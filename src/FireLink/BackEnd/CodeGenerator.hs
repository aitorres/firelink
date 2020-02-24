module FireLink.BackEnd.CodeGenerator where

import           Control.Monad.RWS              (RWST (..), get, put, tell)
import           FireLink.FrontEnd.SymTable     (Dictionary (..),
                                                 DictionaryEntry (..),
                                                 getOffset)
import           FireLink.FrontEnd.TypeChecking
import           TACType

data CodeGenState = CodeGenState
    { cgsNextLabel :: !Int
    , cgsNextTemp :: !Int
    }

initialState :: CodeGenState
initialState = CodeGenState {cgsNextTemp = 0, cgsNextLabel = 0}

type RelativeOffset = Int

data TACSymEntry
    = TACTemporal String
    | TACVariable DictionaryEntry RelativeOffset

instance SymEntryCompatible TACSymEntry where
    getSymID (TACTemporal s)     = s
    getSymID (TACVariable entry rOffset) = "base[" ++ show (getOffset entry + rOffset) ++ "]"

instance Show TACSymEntry where
    show = getSymID

type TAC = ThreeAddressCode TACSymEntry Type
type OperandType = Operand TACSymEntry Type

type CodeGenMonad = RWST Dictionary [TAC] CodeGenState IO

newtemp :: CodeGenMonad TACSymEntry
newtemp = do
    state@CodeGenState {cgsNextTemp = temp} <- get
    put $ state{cgsNextTemp = temp + 1}
    return $ TACTemporal $ "_t" ++ show temp

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
