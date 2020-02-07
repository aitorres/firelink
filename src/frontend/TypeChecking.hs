module TypeChecking where

import Data.List (sort)
import qualified Tokens as T

newtype PropType = PropType (String, Type)
  deriving Show

data Type
  = BigIntT
  | SmallIntT
  | TrileanT
  | FloatT
  | CharT
  | StringT
  | VoidT
  | ArrayT Type
  | SetT Type
  | RecordT Int [PropType]
  | UnionT Int [PropType]
  | PointerT Type
  | FunctionT [Type] Type
  | TypeList [Type]
  | AliasT String
  | Any -- Currently only used in empty set, empty array and null pointers
  | TypeError

instance Show Type where
  show BigIntT = "big humanity"
  show SmallIntT = "small humanity"
  show TrileanT = "bonfire"
  show FloatT = "hollow"
  show CharT = "sign"
  show StringT = "miracle"
  show VoidT = "abyss"
  show (ArrayT t) = "chest of type " ++ show t
  show (SetT t) = "armor of type " ++ show t
  show (RecordT _ ps) = "link with elements " ++ formattedElements
    where elements = concatMap (\(PropType (a, t)) -> a ++ " of type " ++ show t ++ ", ") ps
          usefulChars = length elements - 2
          formattedElements = take usefulChars elements
  show (UnionT _ ps) = "link with elements " ++ formattedElements
    where elements = concatMap (\(PropType (a, t)) -> a ++ " of type " ++ show t ++ ", ") ps
          usefulChars = length elements - 2
          formattedElements = take usefulChars elements
  show (PointerT t) = "arrow to " ++ show t
  show (FunctionT as r) = "spell requiring " ++ concatMap (\t -> show t ++ ", ") as ++ " and returning " ++ show r
  show (TypeList ts) = "typelist " ++ concatMap (\t -> show t ++ ", ") ts
  show (AliasT t) = "knight to " ++ t
  show Any = "any"
  show TypeError = "error type (if you see this, open an issue on github)"

instance Eq Type where
  BigIntT == BigIntT = True
  SmallIntT == SmallIntT = True
  TrileanT == TrileanT = True
  FloatT == FloatT = True
  CharT == CharT = True
  StringT == StringT = True
  ArrayT t == ArrayT t' = t == t'
  SetT t == SetT t' = t == t'
  UnionT s pt == UnionT s' pt' = sort pt == sort pt' && s == s'
  RecordT s pt == RecordT s' pt' = sort pt == sort pt' && s == s'
  PointerT t == PointerT t' = t == t'
  FunctionT ts t == FunctionT ts' t' = ts == ts' && t == t'
  TypeList t == TypeList t' = t == t'
  AliasT s == AliasT s' = s == s'
  TypeError == TypeError = True
  Any == _ = True
  _ == Any = True
  _ == _ = False

instance Ord Type where
  SmallIntT <= SmallIntT = True
  SmallIntT <= BigIntT = True
  SmallIntT <= FloatT = True
  BigIntT <= BigIntT = True
  BigIntT <= FloatT = True
  FloatT <= FloatT = True

  TrileanT <= TrileanT = True
  _ <= _ = False

instance Eq PropType where
  PropType (s, t) == PropType (s', t') = s == s' && t == t'

instance Ord PropType where
  PropType (s, _) <= PropType (s', _) = s <= s'

getTypeFromContainer :: Type -> Maybe Type
getTypeFromContainer (SetT t) = Just t
getTypeFromContainer (ArrayT t) = Just t
getTypeFromContainer _ = Nothing

booleanSingleton :: [Type]
booleanSingleton = [TrileanT]

arithmeticTypes :: [Type]
arithmeticTypes = [BigIntT, SmallIntT, FloatT]

integerTypes :: [Type]
integerTypes = [BigIntT, SmallIntT]

comparableTypes :: [Type]
comparableTypes = [BigIntT, SmallIntT, FloatT]

anySingleton :: [Type]
anySingleton = [Any]

anySetSingleton :: [Type]
anySetSingleton = [SetT Any]

arrayLikeSingleton :: [Type]
arrayLikeSingleton = [ArrayT Any, StringT]

canBeConvertedTo :: Type -> Type -> Bool
SmallIntT `canBeConvertedTo` BigIntT = True
SmallIntT `canBeConvertedTo` FloatT = True
BigIntT `canBeConvertedTo` FloatT = True
SetT a `canBeConvertedTo` SetT b = a `canBeConvertedTo` b
ArrayT a `canBeConvertedTo` ArrayT b = a `canBeConvertedTo` b
a `canBeConvertedTo` b = a == b

typeMismatchMessage :: T.Token -> String
typeMismatchMessage t =
  "Type error near " ++ show t ++ ": " ++ typeMismatchInfo t

numericMismatchMessage :: String
numericMismatchMessage = "numeric operands expected"

integerMismatchMessage :: String
integerMismatchMessage = "integer operands expected"

booleanMismatchMessage :: String
booleanMismatchMessage = "boolean operands expected"

comparableMismatchMessage :: String
comparableMismatchMessage = "same-type operands expected"

collectionMismatchMessage :: String
collectionMismatchMessage = "compatible collection-type operands expected"

typeMismatchInfo :: T.Token -> String
typeMismatchInfo T.Token{T.aToken = T.TkPlus} = numericMismatchMessage
typeMismatchInfo T.Token{T.aToken = T.TkMinus} = numericMismatchMessage
typeMismatchInfo T.Token{T.aToken = T.TkMult} = numericMismatchMessage
typeMismatchInfo T.Token{T.aToken = T.TkDiv} = numericMismatchMessage
typeMismatchInfo T.Token{T.aToken = T.TkMod} = integerMismatchMessage
typeMismatchInfo T.Token{T.aToken = T.TkLt} = comparableMismatchMessage
typeMismatchInfo T.Token{T.aToken = T.TkGt} = comparableMismatchMessage
typeMismatchInfo T.Token{T.aToken = T.TkLte} = comparableMismatchMessage
typeMismatchInfo T.Token{T.aToken = T.TkGte} = comparableMismatchMessage
typeMismatchInfo T.Token{T.aToken = T.TkEq} = comparableMismatchMessage
typeMismatchInfo T.Token{T.aToken = T.TkNeq} = comparableMismatchMessage
typeMismatchInfo T.Token{T.aToken = T.TkAnd} = booleanMismatchMessage
typeMismatchInfo T.Token{T.aToken = T.TkOr} = booleanMismatchMessage
typeMismatchInfo T.Token{T.aToken = T.TkNot} = booleanMismatchMessage
typeMismatchInfo T.Token{T.aToken = T.TkConcat} = collectionMismatchMessage
typeMismatchInfo _ = "(oops! there should be a message here, please fill an issue on Github)"