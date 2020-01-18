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
  | RecordT [PropType]
  | UnionT [PropType]
  | PointerT Type
  | FunctionT [Type] Type
  | TypeList [Type]
  | AliasT String
  | Any -- Currently only used in empty set, empty array and null pointers
  | TypeError
  deriving Show

instance Eq Type where
  BigIntT == BigIntT = True
  SmallIntT == SmallIntT = True
  TrileanT == TrileanT = True
  FloatT == FloatT = True
  CharT == CharT = True
  StringT == StringT = True
  ArrayT t == ArrayT t' = t == t'
  SetT t == SetT t' = t == t'
  UnionT pt == UnionT pt' = sort pt == sort pt'
  RecordT pt == RecordT pt' = sort pt == sort pt'
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

anyArraySingleton :: [Type]
anyArraySingleton = [ArrayT Any]

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