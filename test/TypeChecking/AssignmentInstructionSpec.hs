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

baseFunctionProgram :: String -> String -> String -> String
baseFunctionProgram fT fR vT = "hello ashen one\n\

\invocation f \n\
\with skill of type " ++ fT ++ " \n\
\traveling somewhere\n\
\    go back with " ++ fR ++ "\n\
\you died\n\
\after this return to your world\n\

\traveling somewhere \n\
\with\n\
\   var x of type " ++ vT ++ "\n\
\in your inventory\n\
\   x <<= summon f\n\
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

    it "should accept an assignment of a function that matches type" $
      U.shouldNotError $ baseFunctionProgram "humanity" "3" "humanity"

    it "should accept an assignment of a function that returns a castable type" $
      U.shouldNotError $ baseFunctionProgram "small humanity" "3" "big humanity"

    it "should reject an assignment of a function that can't match or cast type" $
      baseFunctionProgram "hollow" "3.4" "sign" `U.shouldErrorOn` ("<<=", 12, 6)
