module IfInstructionSpec where

import Test.Hspec
import qualified TestUtils as U
import qualified Tokens as T
import qualified SymTable as ST

checkShouldNotError :: String -> IO ()
checkShouldNotError program = U.extractErrors program >>= (`shouldSatisfy` null)

checkShouldErrorOn :: String -> (String, Int, Int) -> IO ()
checkShouldErrorOn program (i, row, col) = do
    errors <- U.extractErrors program
    errors `shouldNotSatisfy` null
    let [ST.SemanticError _ T.Token{T.posn=posn, T.cleanedString=i'}] = errors
    i' `shouldBe` i
    T.row posn `shouldBe` row
    T.col posn `shouldBe` col


spec :: Spec
spec =
    describe "`if` statements" $ do
        it "should accept a program with boolean guards" $
            checkShouldNotError "\
                \hello ashen one\n\

                \traveling somewhere \n\
                \trust your inventory\n\
                \   lit:\n\
                \       traveling somewhere\n\
                \           with orange saponite say @test@\n\
                \       you died\n\
                \inventory closed\n\
                \you died \

                \farewell ashen one"

        it "should reject a program with non-boolean guards" $
            "hello ashen one\n\

            \traveling somewhere \n\
            \trust your inventory\n\
            \   1 + 1:\n\
            \       traveling somewhere\n\
            \           with orange saponite say @test@\n\
            \       you died\n\
            \inventory closed\n\
            \you died \

            \farewell ashen one" `checkShouldErrorOn` ("+", 4, 6)
