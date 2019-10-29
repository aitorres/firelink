module TypeAliasesSpec where

import Test.Hspec
import qualified Utils as U
import qualified SymTable as ST

program :: String -> String
program e = "hello ashen one\

\ requiring help of \
\ " ++ e ++ " \
\ help received \

\ traveling somewhere \
\ go back \
\ you died \
\ farewell ashen one"

spec :: Spec
spec = describe "Variable Declarations" $
    it "allows defining one alias" $ do
        (_, (dict, _, _), _) <- U.extractSymTable $ program "knight x humanity"
        let chain = filter (\d -> ST.name d == "x") $ ST.findChain "x" dict
        chain `shouldNotSatisfy` null
        let entry = head chain
        ST.name entry `shouldBe` "x"
        ST.category entry `shouldBe` ST.Type
        ST.scope entry `shouldBe` 1
        ST.entryType entry `shouldSatisfy` (\Nothing -> True)
        let extra' = ST.extra entry
        extra' `shouldSatisfy` (==1) . length
        U.extractSimpleFromExtra extra' `shouldSatisfy`
            (\(ST.Simple "humanity") -> True)

