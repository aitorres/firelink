module AssignmentInstructionSpec where

import Test.Hspec
import qualified TestUtils as U

baseProgram :: String -> String -> String -> String
baseProgram c t e = "hello ashen one\n\

\traveling somewhere \n\
\with\n\
\   " ++ c ++ " x of type " ++ t ++ "\n\
\in your inventory\n\
\   x <<= " ++ e ++ "\n\
\you died\n\

\farewell ashen one"

spec :: Spec
spec =
  describe "`assignments`" $ do
    it "should accept an assignment with an exactly-matching type" $
      U.shouldNotError $ baseProgram "var" "humanity" "3"

    it "should accept an assignment with an castable type" $
      U.shouldNotError $ baseProgram "var" "hollow" "3"

    it "should reject an assignment with a non-castable type" $
      baseProgram "var" "bonfire" "3" `U.shouldErrorOn` ("<<=", 6, 6)

    it "should reject an assignment to a constant" $
      baseProgram "const" "bonfire <<= lit" "lit" `U.shouldErrorOn` ("<<=", 6, 4)