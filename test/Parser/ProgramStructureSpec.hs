module Parser.ProgramStructureSpec where

import Test.Hspec
import Parser (parse)
import AST
import Data.Maybe
import Lexer

isParseError :: AST f -> Bool
isParseError (ValidAST _) = False
isParseError _ = True

spec :: Spec
spec = describe "ProgramStructure" $
    it "rejects `empty program` as a valid program" $ do
        let program = "hello ashen one farewell ashen one"
        tokens <- scanTokens program
        let ast = parse $ fromJust tokens
        isParseError ast `shouldSatisfy` id
