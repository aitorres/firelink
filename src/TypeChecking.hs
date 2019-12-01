module TypeChecking where

import qualified Grammar as G
import qualified Control.Monad.RWS as RWS
import qualified Lexer as L
import Data.List (sort)

newtype PropType = PropType (String, Type)
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
  | EnumT [String]
  | RecordT [PropType]
  | UnionT [PropType]
  | PointerT Type
  | FunctionT [Type] Type
  | TypeError

instance Eq Type where
  BigIntT == BigIntT = True
  SmallIntT == SmallIntT = True
  TrileanT == TrileanT = True
  FloatT == FloatT = True
  CharT == CharT = True
  StringT == StringT = True
  ArrayT t == ArrayT t' = t == t'
  SetT t == SetT t' = t == t'
  EnumT pt == EnumT pt' = sort pt == sort pt'
  RecordT pt == RecordT pt' = sort pt == sort pt'
  PointerT t == PointerT t' = t == t'
  TypeError == TypeError = True
  _ == _ = False

instance Eq PropType where
  PropType (s, t) == PropType (s', t') = s == s' && t == t'

instance Ord PropType where
  PropType (s, _) <= PropType (s', _) = s <= s'
