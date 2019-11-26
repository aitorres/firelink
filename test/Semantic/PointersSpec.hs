module PointersSpec where

import Test.Hspec
import qualified Utils as U
import qualified SymTable as ST

varEntry :: ST.DictionaryEntry
varEntry = ST.DictionaryEntry
    { ST.scope = 1
    , ST.category = ST.Constant
    , ST.entryType = Just "arrow"
    , ST.name = "x"
    , ST.extra = []
    }

spec :: Spec
spec =
    describe "Functions declarations" $
        it "allows to declare variables with pointers data types" $ do
            let p = "hello ashen one\n\

            \traveling somewhere\n\
            \with\n\
            \   const x of type arrow to sign\n\
            \in your inventory\n\
            \ with orange saponite say @hello world@\n\
            \you died\n\

            \farewell ashen one\n"
            (_, (dict, _, _), _) <- U.extractSymTable p
            U.testEntry dict varEntry U.extractRecursiveFromExtra
                (\(ST.Recursive "arrow" (ST.Simple "sign")) -> True)
