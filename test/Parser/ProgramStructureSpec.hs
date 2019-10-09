module Parser.ProgramStructureSpec where

import Test.Hspec
import Parser.Utils
import Parser
import Data.Maybe
import Lexer

spec :: Spec
spec = describe "ProgramStructure" $ do
    it "rejects `program without main` as a valid program" $ do
        let program = "hello ashen one farewell ashen one"
        tokens <- scanTokens program
        let ast = parse $ fromJust tokens
        isParseError ast `shouldSatisfy` id

    it "accepts a program with an empty main" $ do
        let program = "\
        \ hello ashen one\

        \ traveling somewhere \
        \ you died \

        \ farewell ashen one"
        tokens <- scanTokens program
        let ast = extractValidAST $ parse $ fromJust tokens
        ast `shouldSatisfy` (\(Program [] [] (CodeBlock [] [])) -> True)

