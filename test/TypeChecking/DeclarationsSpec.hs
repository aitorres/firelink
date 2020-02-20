module DeclarationsSpec where

import           Test.Hspec
import qualified TestUtils  as U

baseProgram :: String -> String
baseProgram d = "hello ashen one\n\

\traveling somewhere \n\
\with\n\
\   " ++ d ++ "\n\
\in your inventory\n\
\   go back\n\
\you died\n\

\farewell ashen one"

spec :: Spec
spec =
  describe "`declarations`" $ do
    it "should accept valid simultaneous declarations of the same type" $
      U.shouldNotError $ baseProgram "\
      \var x of type small humanity <<= 1, \n\
      \var y of type small humanity <<= x"

    it "should accept valid simultaneous declarations of castable type" $
      U.shouldNotError $ baseProgram "\
      \var x of type small humanity <<= 1, \n\
      \var y of type big humanity <<= x"

    it "should accept simultaneous declarations using an uninitialized variable (using default value)" $
      U.shouldNotError $ baseProgram "\
      \var x of type big humanity, \n\
      \var y of type big humanity <<= x"

    it "should reject simultaneous declarations of invalid types" $
      baseProgram "\
      \var x of type sign <<= |a|, \n\
      \var y of type small humanity <<= x" `U.shouldErrorOn` ("<<=", 5, 30)

    it "should reject simultaneous declarations of uncastable type" $
      baseProgram "\
      \var x of type big humanity <<= 1, \n\
      \var y of type small humanity <<= x" `U.shouldErrorOn` ("<<=", 5, 30)

    it "should reject simultaneous declarations of matching types in invalid order" $
      baseProgram "\
      \var y of type big humanity <<= x, \n\
      \var x of type big humanity <<= 1" `U.shouldErrorOn` ("<<=", 4, 35)

