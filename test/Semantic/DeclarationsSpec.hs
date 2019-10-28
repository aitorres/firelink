module DeclarationsSpec where

import Test.Hspec
import qualified Utils as U
import qualified SymTable as ST
import qualified Control.Monad.RWS as RWS
import qualified Grammar as G

program :: String -> String
program e = "hello ashen one\
\ traveling somewhere \
\ with \
\   var x of type " ++ e ++ "\
\ in your inventory \
\ go back \
\ you died \
\ farewell ashen one"

commonTest :: String -> String -> ST.Scope -> IO ST.DictionaryEntry
commonTest programType symType s = do
    (_, (dict, _, _), _) <- U.extractSymTable $ program programType
    let chain = filter (\d -> ST.name d == "x") $ ST.findChain "x" dict
    chain `shouldNotSatisfy` null
    let entry = head chain
    ST.name entry `shouldBe` "x"
    ST.category entry `shouldBe` ST.Variable
    ST.scope entry `shouldBe` s
    ST.entryType entry `shouldSatisfy` (\(Just d) -> ST.name d == symType)
    return entry

testSimple :: String -> String -> ST.Scope -> IO ()
testSimple p t s = RWS.void $ commonTest p t s


spec :: Spec
spec = describe "Variable Declarations" $ do
    it "allows declare variables of type `humanity`" $
        testSimple "humanity" "humanity" 1
    it "allows declare variables of type `small humanity`" $
        testSimple "small humanity" "small humanity" 1
    it "allows declare variables of type `hollow`" $
        testSimple "hollow" "hollow" 1
    it "allows declare variables of type `sign`" $
        testSimple "sign" "sign" 1
    it "allows declare variables of type `bonfire`" $
        testSimple "bonfire" "bonfire" 1
    it "allows declare variables of type `<n>-miracle`" $ do
        let (p, t, s) = ("<1>-miracle", ">-miracle", 1)
        testSimple p t s
        entry <- commonTest p t s
        ST.extra entry `shouldSatisfy` (\l -> length l == 2)
        let size = U.extractSizeFromExtra $ ST.extra entry
        let constructor = U.extractConstructorFromExtra $ ST.extra entry
        size `shouldSatisfy` (\(G.IntLit 1) -> True)
        constructor `shouldSatisfy` (\(ST.DictionaryEntry{ST.name=">-miracle"}) -> True)
