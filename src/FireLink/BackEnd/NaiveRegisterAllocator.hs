module FireLink.BackEnd.NaiveRegisterAllocator (
    allocateRegisters, initialDescriptorLists
) where

import           Control.Monad.RWS              (RWST (..), ask, get, put, tell,
                                                 unless, when)
import           Data.Char                      (toUpper)
import qualified Data.Map.Strict                as Map
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import           FireLink.BackEnd.CodeGenerator (OperandType (..), TAC (..),
                                                 TACSymEntry (..))
import           FireLink.FrontEnd.SymTable     (DictionaryEntry (..))
import           TACType

-- | Amount of available registers on the target architecture:
-- | ten $sx registers, seven $tx registers
availableRegisters :: Int
availableRegisters = 17

-- | Descriptors are kept in a string->set<string> map (hash table)
type DescriptorDictionary = Map.Map String (Set String)

-- | Datatype to hold both variable and register descriptors
data DescriptorList = DescriptorList
    { dlVarDescriptors :: !DescriptorDictionary
    , dlRegDescriptors :: !DescriptorDictionary
    }
instance Show DescriptorList where
    show DescriptorList { dlVarDescriptors = dVars, dlRegDescriptors = dRegs} =
        "Descriptor Lists\nVariables:\n\t" ++ show dVars ++ "\nRegisters:\n\t" ++ show dRegs

-- | A RWST-compatible monad to read a TAC block, write lists of allocated
-- | registers, and keep variable/register descriptors on state while
-- | performing IO operations.
type NaiveRegisterAllocatorMonad = RWST [TAC] [String] DescriptorList IO

-- | Returns a descriptor list in which the variable descriptors are an empty map,
-- | given that they should be filled on a case-basis, and having the register
-- | descriptors hold an initially-empty list for each available register
initialDescriptorLists :: DescriptorList
initialDescriptorLists = DescriptorList
    { dlVarDescriptors = Map.empty
    , dlRegDescriptors = Map.fromList $ zip ["_R" ++ show x | x <- [1..availableRegisters]] $ repeat Set.empty
    }

-- | Running as a RWST-called function, fully-initializes
-- | the descriptor list, and then allocates registers for
-- | each TAC instruction
allocateRegisters :: NaiveRegisterAllocatorMonad ()
allocateRegisters = do
    tacList <- ask
    mapM_ generateVarDescriptors tacList
    DescriptorList { dlVarDescriptors=a, dlRegDescriptors=b }<- get
    unless (a == Map.empty) $ mapM_ getReg tacList

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
                    return $ Map.insert vName (Set.singleton vName) vDescs
                TACTemporal s _ -> return $ Map.insert s Set.empty vDescs
            put $ ds { dlVarDescriptors=vDescs' }
        generateVarDescriptor _ = return ()

getRegisterForVar :: String -> NaiveRegisterAllocatorMonad (Maybe String)
getRegisterForVar v = do
    DescriptorList { dlRegDescriptors=rDescs } <- get
    let registersWithV = Map.filter (Set.member v) rDescs
    if registersWithV == Map.empty
    then return Nothing
    else return $ Just $ fst (Map.elemAt 0 registersWithV)

getEmptyRegister :: NaiveRegisterAllocatorMonad (Maybe String)
getEmptyRegister = do
    DescriptorList { dlRegDescriptors=rDescs } <- get
    let emptyRegDescs = Map.filter (Set.empty ==) rDescs
    if emptyRegDescs == Map.empty
    then return Nothing
    else return $ Just $ fst (Map.elemAt 0 emptyRegDescs)

setRegister :: String -> String -> NaiveRegisterAllocatorMonad ()
setRegister r v = do
    dl@DescriptorList { dlRegDescriptors=rDescs } <- get
    let rDescs' = Map.insert r (Set.singleton v) rDescs
    put $ dl { dlRegDescriptors = rDescs' }

setVariable :: String -> String -> NaiveRegisterAllocatorMonad ()
setVariable v r = do
    dl@DescriptorList { dlVarDescriptors=vDescs } <- get
    let vDescs' = Map.insert v (Set.singleton r) vDescs
    put $ dl { dlVarDescriptors = vDescs' }

addToRegister :: String -> String -> NaiveRegisterAllocatorMonad ()
addToRegister r v = do
    dl@DescriptorList { dlRegDescriptors=rDescs } <- get
    let rVars = Map.findWithDefault Set.empty r rDescs
    let rDescs' = Map.insert r (Set.insert v rVars) rDescs
    put $ dl { dlRegDescriptors = rDescs' }

addToVariable :: String -> String -> NaiveRegisterAllocatorMonad ()
addToVariable v r = do
    dl@DescriptorList { dlVarDescriptors=vDescs } <- get
    let vRegs = Map.findWithDefault Set.empty v vDescs
    let vDescs' = Map.insert v (Set.insert r vRegs) vDescs
    put $ dl { dlVarDescriptors = vDescs' }

getVarName :: OperandType -> String
getVarName (Id v) = case v of
    TACVariable entry _ -> name entry
    TACTemporal s _     -> s

removeRegister :: String -> String -> NaiveRegisterAllocatorMonad ()
removeRegister rA vA = do
    dl@DescriptorList { dlVarDescriptors=vDescs } <- get
    let vDescs' = Map.mapWithKey (\k x -> if k /= vA then Set.delete rA x else x) vDescs
    put dl { dlVarDescriptors=vDescs' }

-- | TODO: Finish and clean implementation
getReg :: TAC -> NaiveRegisterAllocatorMonad ()
getReg t@(ThreeAddressCode op (Just a) (Just b) (Just c)) =
    if op `elem` [Add, Sub, Mult, Div] then do
        let nameA = getVarName a
        let nameB = getVarName b
        let nameC = getVarName c
        regB <- getIndividualReg nameB True
        regC <- getIndividualReg nameC True
        regA <- getIndividualReg nameA False
        tell ["\t" ++ map toUpper (show op) ++ " " ++ regA ++ ", " ++ regB ++ ", " ++ regC]
        setRegister regA nameA
        setVariable nameA regA
        removeRegister regA nameA
    else tell [show t ++ " (not supported by naiveRegisterAllocator)"]
getReg (ThreeAddressCode NewLabel _ (Just l) _ ) = tell [show l ++ ":"]
getReg (ThreeAddressCode op Nothing Nothing Nothing) = tell ["\t" ++ map toUpper (show op)]
getReg t = tell [show t ++ " (not supported by naiveRegisterAllocator)"]

getIndividualReg :: String -> Bool -> NaiveRegisterAllocatorMonad String
getIndividualReg n upd = do
    gr <- getRegisterForVar n
    case gr of
        Just reg1 -> return reg1
        Nothing -> do
            gre <- getEmptyRegister
            case gre of
                Just reg2 -> do
                    when upd $ do
                        -- Should not use n but the best possible current container of v
                        tell ["\tLD " ++ reg2 ++ ", " ++ n]
                        setRegister reg2 n
                        addToVariable n reg2
                    return reg2
                Nothing -> error "CASE NOT IMPLEMENTED"

