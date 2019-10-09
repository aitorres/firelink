module ExpressionsSpec where

import Test.Hspec
import Utils
import Grammar

buildProgramWithExpr e = "\
\ hello ashen one \

\ traveling somewhere \
\   with \
\      const patata of type humanity <<= " ++ e ++ " \
\   in your inventory \
\   with orange saponite say @Hello world@ \
\ you died \

\ farewell ashen one"
