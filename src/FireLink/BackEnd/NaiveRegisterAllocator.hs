module FireLink.BackEnd.NaiveRegisterAllocator (
    allocateRegisters, initialDescriptorLists
) where

import           Control.Monad.RWS              (RWST (..), ask, get, put, tell,
                                                 unless)
import qualified Data.Map.Strict                as Map
import           FireLink.BackEnd.CodeGenerator (OperandType (..), TAC (..),
                                                 TACSymEntry (..))
import           FireLink.FrontEnd.SymTable     (DictionaryEntry (..))
import           TACType

-- | Amount of available registers on the target architecture:
-- | ten $sx registers, seven $tx registers
availableRegisters :: Int
availableRegisters = 17

-- | Descriptors are kept in a string->string map (hash table)
type DescriptorDictionary = Map.Map String [String]

-- | Datatype to hold both variable and register descriptors
data DescriptorList = DescriptorList
    { dlVarDescriptors :: !DescriptorDictionary
    , dlRegDescriptors :: !DescriptorDictionary
    }
instance Show DescriptorList where
    show DescriptorList { dlVarDescriptors = dVars, dlRegDescriptors = dRegs} =
        "Descriptor Lists\nVariables:\n\t" ++ show dVars ++ "\nRegisters:\n\t" ++ show dRegs

-- | A 3-tuple of registers to use on each instruction's operand
type AllocatedRegister = (Maybe String, Maybe String, Maybe String)

-- | Semantic shortcut for a list of allocated registers
type AllocatedRegisters = [AllocatedRegister]

-- | A RWST-compatible monad to read a TAC block, write lists of allocated
-- | registers, and keep variable/register descriptors on state while
-- | performing IO operations.
type NaiveRegisterAllocatorMonad = RWST [TAC] AllocatedRegisters DescriptorList IO

-- | Returns a descriptor list in which the variable descriptors are an empty map,
-- | given that they should be filled on a case-basis, and having the register
-- | descriptors hold an initially-empty list for each available register
initialDescriptorLists :: DescriptorList
initialDescriptorLists = DescriptorList
    { dlVarDescriptors = Map.empty
    , dlRegDescriptors = Map.fromList $ zip ["_R" ++ show x | x <- [1..availableRegisters]] $ repeat []
    }

-- | Running as a RWST-called function, fully-initializes
-- | the descriptor list, and then allocates registers for
-- | each TAC instruction
allocateRegisters :: NaiveRegisterAllocatorMonad ()
allocateRegisters = do
    tacList <- ask
    mapM_ generateVarDescriptors tacList
    DescriptorList { dlVarDescriptors=a, dlRegDescriptors=b }<- get
    unless (a == Map.empty) $ mapM_ allocateRegistersForTac tacList

-- | Given a TAC instruction, analyzes and maps variables and temporals
-- | to variable descriptors
generateVarDescriptors :: TAC -> NaiveRegisterAllocatorMonad ()
generateVarDescriptors (ThreeAddressCode _ a b c) =
    mapM_ generateVarDescriptor [a, b, c]
    where
        generateVarDescriptor :: Maybe OperandType -> NaiveRegisterAllocatorMonad ()
        generateVarDescriptor (Just (Id a)) = do
            ds@DescriptorList { dlVarDescriptors=vDescs } <- get
            vDescs' <- case a of
                TACVariable entry _ -> do
                    let vName = name entry
                    return $ Map.insert vName [vName] vDescs
                TACTemporal s _ -> return $ Map.insert s [] vDescs
            put $ ds { dlVarDescriptors=vDescs' }
        generateVarDescriptor _ = return ()

-- | TODO: Implement
allocateRegistersForTac :: TAC -> NaiveRegisterAllocatorMonad ()
allocateRegistersForTac tac = return ()
