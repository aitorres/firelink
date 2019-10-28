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

test :: String -> ST.Scope -> IO ()
test t s = do
    (_, (dict, _, _), _) <- U.extractSymTable $ program t
    let chain = filter (\d -> ST.name d == "x") $ ST.findChain "x" dict
    chain `shouldNotSatisfy` null
    let entry = head chain
    ST.name entry `shouldBe` "x"
    ST.category entry `shouldBe` ST.Constant
    ST.scope entry `shouldBe` s
    ST.entryType entry `shouldSatisfy` (\(Just d) -> ST.name d == t)


spec :: Spec
spec = describe "Variable Declarations" $ do
    it "allows declare variables of type `humanity`" $ test "humanity" 1
    it "allows declare variables of type `small humanity`" $ test "small humanity" 1
    it "allows declare variables of type `hollow`" $ test "hollow" 1
    it "allows declare variables of type `sign`" $ test "sign" 1
    it "allows declare variables of type `bonfire`" $ test "bonfire" 1
