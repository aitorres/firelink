module DeclarationsSpec where

import Test.Hspec
import qualified Utils as U
import qualified SymTable as ST

program :: String -> String
program e = "hello ashen one\
\ traveling somewhere \
\ with \
\   const x of type " ++ e ++ "\
\ in your inventory \
\ go back \
\ you died \
\ farewell ashen one"


spec :: Spec
spec = describe "Variable Declarations" $
    it "allows declare variables of scalar data types" $ do
        (_, (dict, _, _), _) <- U.extractSymTable $ program "humanity"
        print dict
        let chain = filter (\d -> ST.name d == "x") $ ST.findChain "x" dict
        chain `shouldNotSatisfy` null
        let entry = head $ chain
        ST.name entry `shouldBe` "x"
        ST.category entry `shouldBe` ST.Constant
        ST.scope entry `shouldBe` 1
        ST.entryType entry `shouldSatisfy` (\(Just d) -> ST.name d == "humanity")

