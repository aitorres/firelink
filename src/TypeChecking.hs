module TypeChecking where

import qualified SymTable as ST
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
  | ArrayT Type
  | SetT Type
  | EnumT [String]
  | RecordT [PropType]
  | UnionT [PropType]
  | PointerT Type
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

  a /= b = not (a == b)

instance Eq PropType where
  PropType (s, t) == PropType (s', t') = s == s' && t == t'
  a /= b = not (a == b)

instance Ord PropType where
  PropType (s, _) < PropType (s', _) = s < s'
  PropType (s, _) <= PropType (s', _) = s <= s'
  PropType (s, _) > PropType (s', _) = s > s'
  PropType (s, _) >= PropType (s', _) = s > s'
  a@(PropType (s, _)) `max` b@(PropType (s', _)) =
    if s `max` s' == s
      then a
      else b
  a@(PropType (s, _)) `min` b@(PropType (s', _)) =
    if s `min` s' == s
      then a
      else b

class TypeCheckable a where
  getType :: a -> ST.ParserMonad Type
  typeMatches :: a -> a -> ST.ParserMonad Bool
  typeMatches a b = do
    aType <- getType a
    bType <- getType b
    return (aType == bType)

isOneOfTypes :: [Type] -> G.Expr -> ST.ParserMonad Bool
isOneOfTypes ts a = do
  t <- getType a
  let isValidType = or [t == x | x <- ts]
  return isValidType

isLogicalType :: G.Expr -> ST.ParserMonad Bool
isLogicalType = isOneOfTypes [TrileanT]

isNumberType :: G.Expr -> ST.ParserMonad Bool
isNumberType = isOneOfTypes [BigIntT, SmallIntT, FloatT]

isComparableType :: G.Expr -> ST.ParserMonad Bool
isComparableType = isOneOfTypes [BigIntT, SmallIntT, FloatT]

isEquatableType :: G.Expr -> ST.ParserMonad Bool
isEquatableType = isOneOfTypes [BigIntT, SmallIntT, FloatT, CharT, TrileanT]

isShowableType :: G.Expr -> ST.ParserMonad Bool
isShowableType = isOneOfTypes [CharT, StringT]

conditionalCheck :: (G.Expr -> ST.ParserMonad Bool) -> G.Expr -> G.Expr ->  ST.ParserMonad Type
conditionalCheck condition a b = do
  matches <- typeMatches a b
  aType <- getType a
  isValidType <- condition a
  return (
    if matches && isValidType then
      aType
    else
      TypeError
    )

conditionalCheckReturningBonfire ::  (G.Expr -> ST.ParserMonad Bool) -> G.Expr -> G.Expr ->  ST.ParserMonad Type
conditionalCheckReturningBonfire condition a b = do
  matchingType <- conditionalCheck condition a b
  return (
    if matchingType == TypeError then
      TypeError
      else
        TrileanT
        )

arithmeticCheck :: G.Expr -> G.Expr -> ST.ParserMonad Type
arithmeticCheck = conditionalCheck isNumberType

comparableCheck :: G.Expr -> G.Expr -> ST.ParserMonad Type
comparableCheck = conditionalCheckReturningBonfire isComparableType

equatableCheck :: G.Expr -> G.Expr -> ST.ParserMonad Type
equatableCheck = conditionalCheckReturningBonfire isEquatableType

logicalCheck :: G.Expr -> G.Expr -> ST.ParserMonad Type
logicalCheck = conditionalCheckReturningBonfire isLogicalType

containerCheck _ = error "not implemented" -- TODO: implement array data type check

instance TypeCheckable G.Expr where
  getType G.TrueLit = return TrileanT
  getType G.FalseLit = return TrileanT
  getType G.UndiscoveredLit = return TrileanT
  getType G.NullLit = return TypeError
  getType (G.IntLit _) = return BigIntT
  getType (G.FloatLit _) = return FloatT
  getType (G.CharLit _) = return CharT
  getType (G.StringLit _) = return StringT
  getType (G.ArrayLit a) = containerCheck a
  getType (G.SetLit a) = containerCheck a
  getType (G.EvalFunc id _) = getType (G.IdExpr id) -- TODO: Check if okay
  getType (G.Add a b) = arithmeticCheck a b
  getType (G.Substract a b) = arithmeticCheck a b
  getType (G.Multiply a b) = arithmeticCheck a b
  getType (G.Divide a b) = arithmeticCheck a b
  getType (G.Mod a b) = arithmeticCheck a b
  getType (G.Negative a) = arithmeticCheck a a -- cheating
  getType (G.Lt a b) = comparableCheck a b
  getType (G.Lte a b) = comparableCheck a b
  getType (G.Gt a b) = comparableCheck a b
  getType (G.Gte a b) = comparableCheck a b
  getType (G.Eq a b) = equatableCheck a b
  getType (G.Neq a b) = equatableCheck a b
  getType (G.And a b) = logicalCheck a b
  getType (G.Or a b) = logicalCheck a b
  getType (G.Not a) = logicalCheck a a -- cheating
  getType (G.Access e i) = return TypeError -- TODO: Accessor type
  getType (G.IndexAccess e1 e2) = return TypeError -- TODO: Accessor type
  getType (G.MemAccess e) = return TypeError -- TODO: Mem access
  getType _ = return TypeError -- TODO: Finish implementation

checkTypes :: G.Expr -> G.Expr -> String -> L.Token -> ST.ParserMonad ()
checkTypes a b t tk = do
  let isError = t == ST.errorType
  RWS.when isError $ RWS.tell [ST.SemanticError ("Type mismatch between " ++ show a ++ " and " ++ show b) tk]

checkType :: G.Expr -> String -> L.Token -> ST.ParserMonad ()
checkType a t tk = do
  let isError = t == ST.errorType
  RWS.when isError $ RWS.tell [ST.SemanticError ("Type mismatch for " ++ show a) tk]
