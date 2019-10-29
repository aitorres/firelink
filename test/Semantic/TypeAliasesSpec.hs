module TypeAliasesSpec where

import Test.Hspec
import qualified Utils as U
import qualified SymTable as ST
import qualified Grammar as G

program :: String -> String
program e = "hello ashen one\

\ requiring help of \
\ " ++ e ++ " \
\ help received \

\ traveling somewhere \
\ go back \
\ you died \
\ farewell ashen one"

test' :: String -> ST.Scope -> ([ST.Extra] -> ST.Extra) -> (ST.Extra -> Bool) -> IO ()
test' prog scope extractor predicate = do
    let varName = "x"
    (_, (dict, _, _), _) <- U.extractSymTable prog
    let chain = filter (\d -> ST.name d == varName) $ ST.findChain varName dict
    chain `shouldNotSatisfy` null
    let entry = head chain
    ST.name entry `shouldBe` varName
    ST.category entry `shouldBe` ST.Type
    ST.scope entry `shouldBe` scope
    ST.entryType entry `shouldSatisfy` (\Nothing -> True)
    let extra' = ST.extra entry
    extra' `shouldSatisfy` (==1) . length
    extractor extra' `shouldSatisfy` predicate


test :: String -> ST.Scope -> ([ST.Extra] -> ST.Extra) -> (ST.Extra -> Bool) -> IO ()
test = test' . program

spec :: Spec
spec = describe "Variable Declarations" $ do
    it "allows to define aliases to just primitive types" $
        test "knight x humanity" 1 U.extractSimpleFromExtra
            (\(ST.Simple "humanity") -> True)
    it "allows to define aliases to data types with size (strings alikes)" $
        test "knight x <12>-miracle" 1 U.extractCompoundFromExtra
            (\(ST.Compound ">-miracle" (G.IntLit 12)) -> True)
