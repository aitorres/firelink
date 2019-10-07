module AST (AST (..)) where

data AST a
  = ValidAST a
  | InvalidAST String

instance Show a => Show (AST a) where
  show (ValidAST a) = show a
  show (InvalidAST msg) = msg

instance Functor AST where
  fmap f (ValidAST a) = ValidAST (f a)
  fmap f (InvalidAST b) = InvalidAST b

instance Applicative AST where
  pure = ValidAST
  (ValidAST f) <*> m = fmap f m
  (InvalidAST msg) <*> _ = InvalidAST msg

instance Monad AST where
  return = ValidAST
  fail = InvalidAST
  (InvalidAST msg) >>= _ = InvalidAST msg
  (ValidAST a) >>= f = f a
