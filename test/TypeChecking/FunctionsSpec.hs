module FunctionsSpec where

import Test.Hspec
import qualified TestUtils as U

spec :: Spec
spec =
    describe "functions" $ do
        it "should accept a program with a properly typed return expression on a function" $
            U.shouldNotError "\
                \hello ashen one\n\
                
                \invocation f\n\
                \with skill of type humanity\n\
                \traveling somewhere\n\
                \    go back with 5\n\
                \you died\n\
                \after this return to your world\n\

                \traveling somewhere \n\
                \go back\n\
                \you died\n\

                \farewell ashen one"

        it "should accept a program with a compatible return expression on a function" $
            U.shouldNotError "\
                \hello ashen one\n\
                
                \invocation f\n\
                \with skill of type hollow\n\
                \traveling somewhere\n\
                \    go back with 5\n\
                \you died\n\
                \after this return to your world\n\

                \traveling somewhere \n\
                \go back\n\
                \you died\n\

                \farewell ashen one"

        it "should reject a program with an improperly typed return expression on a function" $
            "\
            \hello ashen one\n\
            
            \invocation f\n\
            \with skill of type humanity\n\
            \traveling somewhere\n\
            \    go back with |a|\n\
            \you died\n\
            \after this return to your world\n\

            \traveling somewhere \n\
            \go back \n\
            \you died\n\

            \farewell ashen one" `U.shouldErrorOn` ("go back with |a|", 5, 5)

        it "should reject a program with a go-back return without an expression on a function" $
            "\
            \hello ashen one\n\
            
            \invocation f\n\
            \with skill of type humanity\n\
            \traveling somewhere\n\
            \    go back\n\
            \you died\n\
            \after this return to your world\n\

            \traveling somewhere \n\
            \go back\n\
            \you died\n\

            \farewell ashen one" `U.shouldErrorOn` ("go back", 5, 5)

        it "should reject a program with a go-back return with an expression on the main body" $
            "\
            \hello ashen one\n\

            \traveling somewhere \n\
            \go back with 3\n\
            \you died\n\

            \farewell ashen one" `U.shouldErrorOn` ("go back", 3, 1)
