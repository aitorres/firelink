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

