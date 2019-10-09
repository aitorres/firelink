module Parser.DeclarationSpec where

import Test.Hspec
import Parser.Utils
import Parser
import Data.Maybe
import Lexer

spec :: Spec
spec = describe "Declarations for Block" $
    it "accepts empty declaration block" $ do
        let program = buildProgramWithEmptyMain ""
        tokens <- scanTokens program
        let ast = extractValidAST tokens
        ast `shouldSatisfy` (\(Program _ _ (CodeBlock [] _)) -> True)
