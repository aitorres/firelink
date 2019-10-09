module Parser.DeclarationSpec where

import Test.Hspec
import Parser.Utils
import Parser

spec :: Spec
spec = describe "Declarations for Block" $
    it "accepts empty declaration block" $
        runTestForValidProgram "" (\(Program _ _ (CodeBlock [] _)) -> True)
