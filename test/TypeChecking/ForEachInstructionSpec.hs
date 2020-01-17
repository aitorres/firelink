module ForEachInstructionSpec where

import Test.Hspec
import qualified TestUtils as U

spec :: Spec
spec =
    describe "`if` statements" $ do
        it "should accept a program with boolean guards" $
            U.shouldNotError "\
                \hello ashen one\n\

                \traveling somewhere \n\
                \with\n\
                \   var i of type sign\n\
                \   var is of type armor of type sign\n\
                \in your inventory\n\
                
                \repairing i with titanite from is\n\
                \traveling somewhere\n\
                    \with orange saponite say i\n\
                \you died\n\
                \weaponry repaired\n\
                \you died\n\

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

            \farewell ashen one" `U.shouldErrorOn` ("+", 4, 6)
