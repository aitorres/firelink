module ProceduresSpec where

import Test.Hspec
import qualified TestUtils as U

spec :: Spec
spec =
    describe "procedures" $ do
        it "should accept a program with a return instruction on a procedure" $
            U.shouldNotError "\
                \hello ashen one\n\
                
                \spell f\n\
                \traveling somewhere\n\
                \    go back\n\
                \you died\n\
                \ashen estus flask consumed\n\

                \traveling somewhere \n\
                \go back\n\
                \you died\n\

                \farewell ashen one"

        it "should reject a program with a typed return expression on a procedure" $
            "\
            \hello ashen one\n\
            
            \spell f\n\
            \traveling somewhere\n\
            \    go back with 4\n\
            \you died\n\
            \ashen estus flask consumed\n\

            \traveling somewhere \n\
            \go back \n\
            \you died\n\

            \farewell ashen one" `U.shouldErrorOn` ("go back with 4", 4, 5)