module ForEachInstructionSpec where

import Test.Hspec
import qualified TestUtils as U

baseProgram :: String -> String
baseProgram vars = "hello ashen one\n\

\traveling somewhere \n\
\with\n\
\   " ++ vars ++ "\n\
\in your inventory\n\
\repairing i with titanite from is\n\
\traveling somewhere\n\
    \with orange saponite say i\n\
\you died\n\
\weaponry repaired\n\
\you died\n\

\farewell ashen one"

spec :: Spec
spec =
    describe "`for-each` statements" $ do
        it "should accept a program with container as an structure and an iterator of the wrapped type" $
            U.shouldNotError $ baseProgram "var i of type sign,\
            \var is of type armor of type sign"

        it "should reject a program with non-container as an structure and an iterator of the wrapped type" $
            baseProgram "var i of type sign,\
            \var is of type sign" `U.shouldErrorOn` ("+", 6, 34)
