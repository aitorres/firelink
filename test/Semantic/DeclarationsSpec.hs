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

commonTest' :: String -> String -> ST.Scope -> String -> IO ST.DictionaryEntry
commonTest' p symType s varName = do
    (_, (dict, _, _), _) <- U.extractSymTable p
    let chain = filter (\d -> ST.name d == varName) $ ST.findChain varName dict
    chain `shouldNotSatisfy` null
    let entry = head chain
    ST.name entry `shouldBe` varName
    ST.category entry `shouldBe` ST.Variable
    ST.scope entry `shouldBe` s
    ST.entryType entry `shouldSatisfy` (\(Just d) -> d == symType)
    return entry

commonTest :: String -> String -> ST.Scope -> IO ST.DictionaryEntry
commonTest programType symType s = commonTest' (program programType) symType s "x"

testSimple :: String -> String -> ST.Scope -> IO ()
testSimple p t s = RWS.void $ commonTest p t s

testCompound :: String -> String -> ST.Scope -> ([ST.Extra] -> ST.Extra) -> (ST.Extra -> Bool) -> IO ()
testCompound p t s extractor predicate = do
    testSimple p t s
    entry <- commonTest p t s
    let extra' = ST.extra entry
    extra' `shouldSatisfy` (\l -> length l == 1)
    print extra'
    extractor extra' `shouldSatisfy` predicate

spec :: Spec
spec = describe "Variable Declarations" $ do
    it "allows to declare variables of type `humanity`" $
        testSimple "humanity" "humanity" 1
    it "allows to declare variables of type `small humanity`" $
        testSimple "small humanity" "small humanity" 1
    it "allows to declare variables of type `hollow`" $
        testSimple "hollow" "hollow" 1
    it "allows to declare variables of type `sign`" $
        testSimple "sign" "sign" 1
    it "allows to declare variables of type `bonfire`" $
        testSimple "bonfire" "bonfire" 1
    it "allows to declare variables of type `<n>-miracle`" $
        testCompound "<1>-miracle" ">-miracle" 1
            U.extractCompoundFromExtra (\(ST.Compound ">-miracle" (G.IntLit 1)) -> True)
    it "allows to declare variables of recursive type `<n>-chest of type humanity" $
        testCompound "<1>-chest of type humanity" ">-chest" 1
        U.extractCompoundRecFromExtra (\(ST.CompoundRec ">-chest"
            (G.IntLit 1)
            (ST.Simple "humanity")) -> True)
    it "allows to declare variables of recursive type `<n>-chest of type <m>-chest of type humanity" $
        testCompound "<1>-chest of type <2>-chest of type humanity" ">-chest" 1
            U.extractCompoundRecFromExtra (\(ST.CompoundRec
                ">-chest"
                (G.IntLit 1)
                (ST.CompoundRec
                    ">-chest"
                    (G.IntLit 2)
                    (ST.Simple "humanity"))) -> True)
    it "allows to declare variables of recursive type `<n>-chest of type <n>-miracle" $
        testCompound "<1>-chest of type <2>-miracle" ">-chest" 1
            U.extractCompoundRecFromExtra (\(ST.CompoundRec
                ">-chest"
                (G.IntLit 1)
                (ST.Compound
                    ">-miracle"
                    (G.IntLit 2))) -> True)
    it "allows to declare variables of recursive type `armor of type sign" $
        testCompound "armor of type sign" "armor" 1
            U.extractRecursiveFromExtra (\(ST.Recursive
                "armor"
                (ST.Simple
                    "sign")) -> True)
    it "allows to declare variables of recursive type `armor of type <n>-chest of type sign" $
        testCompound "armor of type <1>-chest of type sign" "armor" 1
            U.extractRecursiveFromExtra (\(ST.Recursive
                "armor"
                (ST.CompoundRec
                    ">-chest"
                    (G.IntLit 1)
                    (ST.Simple "sign"))) -> True)
    it "allows declare variables of recursive type `armor of type armor of type sign" $
        testCompound "armor of type armor of type sign" "armor" 1
            U.extractRecursiveFromExtra (\(ST.Recursive
                "armor"
                (ST.Recursive
                    "armor"
                    (ST.Simple "sign"))) -> True)

    it "allows to declare 2 or more variables" $
        let p = "hello ashen one\

        \ traveling somewhere \

        \ with \
        \ var x of type sign, \
        \ var y of type humanity \
        \ in your inventory \

        \ go back \
        \ you died \

        \ farewell ashen one" in
        commonTest' p "sign" 1 "x" >>
        commonTest' p "humanity" 1 "y" >>
        return ()
