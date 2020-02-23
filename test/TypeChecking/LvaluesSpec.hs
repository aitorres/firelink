module LvaluesSpec where

import           Test.Hspec
import qualified TestUtils  as U

baseProgram :: String -> String -> String
baseProgram d o = "hello ashen one\n\

\traveling somewhere \n\
\with\n\
\" ++ d ++ "\n\
\in your inventory\n\
\   " ++ o ++ "\n\
\you died\n\

\farewell ashen one"

spec :: Spec
spec =
  describe "`lvalues`" $ do
    it "should accept id as lvalue" $
      U.shouldNotError $ baseProgram "var n of type humanity" "n <<= 1"

    it "should accept memory access as lvalue" $
      U.shouldNotError $ baseProgram "var n of type arrow to humanity" "throw a n <<= 1"

    it "should accept array access as lvalue" $
      U.shouldNotError $ baseProgram "var n of type <4>-chest of type humanity" "n<$2$> <<= 1"

    it "should accept property access as lvalue" $
      U.shouldNotError $ baseProgram "var n of type bezel { x of type humanity }" "n~>x <<= 1"

    it "should reject another exprs as lvalue" $
      baseProgram "var n of type humanity" "1 + 2 <<= 3" `U.shouldErrorOn` ("+", 6, 6)
