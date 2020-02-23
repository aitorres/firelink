module SizeSpec where

import           Test.Hspec
import qualified TestUtils  as U

baseProgram :: String -> String
baseProgram t = "hello ashen one\n\

\traveling somewhere \n\
\with\n\
\   var x of type " ++ t ++ ",\n\
\   var n of type humanity\n\
\in your inventory\n\
\   n <<= size x\n\
\you died\n\

\farewell ashen one"

spec :: Spec
spec =
  describe "`IO`" $ do
    it "should accept a read operation with sets" $
      U.shouldNotError $ baseProgram "armor of type humanity"

    it "should accept a print operation with strings" $
      U.shouldNotError $ baseProgram "<10>-miracle"

    it "should accept a print operation with arrays" $
      U.shouldNotError $ baseProgram "<10>-chest of type humanity"

    it "should reject a read operation with any other type" $
      baseProgram "humanity" `U.shouldErrorOn`  ("<<=", 7, 10)
