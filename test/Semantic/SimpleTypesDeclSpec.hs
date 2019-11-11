module SimpleTypesDeclSpec where

import Test.Hspec
import qualified Utils as U
import qualified SymTable as ST
import qualified Grammar as G
import qualified Lexer as L

program :: String -> String
program e = "hello ashen one\
\ traveling somewhere \
\ with \
\   var x of type " ++ e ++ "\
\ in your inventory \
\ go back \
\ you died \
\ farewell ashen one"

varEntry :: ST.DictionaryEntry
varEntry = ST.DictionaryEntry
    { ST.scope = 1
    , ST.category = ST.Variable
    , ST.entryType = Nothing
    , ST.name = "x"
    , ST.extra = []
    }

test :: U.TestFunction String ST.Dictionary
test prog = U.test (program prog)

testVoid :: U.TestFunction String ()
testVoid prog = U.testVoid (program prog)

spec :: Spec
spec = do
    describe "Simple type variable Declarations" $ do
        it "allows to declare variables of type `humanity`" $
            testVoid "humanity" varEntry{ST.entryType = Just "humanity"}
                U.extractSimpleFromExtra (\(ST.Simple "humanity") -> True)
        it "allows to declare variables of type `small humanity`" $
            testVoid "small humanity" varEntry{ST.entryType = Just "small humanity"}
                U.extractSimpleFromExtra (\(ST.Simple "small humanity") -> True)
        it "allows to declare variables of type `hollow`" $
            testVoid "hollow" varEntry{ST.entryType = Just "hollow"}
                U.extractSimpleFromExtra (\(ST.Simple "hollow") -> True)
        it "allows to declare variables of type `sign`" $
            testVoid "sign" varEntry{ST.entryType = Just "sign"}
                U.extractSimpleFromExtra (\(ST.Simple "sign") -> True)
        it "allows to declare variables of type `bonfire`" $
            testVoid "bonfire" varEntry{ST.entryType = Just "bonfire"}
                U.extractSimpleFromExtra (\(ST.Simple "bonfire") -> True)
        it "allows to declare variables of type `<n>-miracle`" $
            testVoid "<1>-miracle"  varEntry{ST.entryType = Just ">-miracle"} U.extractCompoundFromExtra
                (\(ST.Compound ">-miracle" (G.IntLit 1)) -> True)
        it "allows to declare variables of recursive type `<n>-chest of type humanity" $
            testVoid "<1>-chest of type humanity"  varEntry{ST.entryType = Just ">-chest"}
            U.extractCompoundRecFromExtra
                (\(ST.CompoundRec ">-chest"
                    (G.IntLit 1)
                    (ST.Simple "humanity")) -> True)
        it "allows to declare variables of recursive type `<n>-chest of type <m>-chest of type humanity" $
            testVoid "<1>-chest of type <2>-chest of type humanity" varEntry{ST.entryType = Just ">-chest"}
                U.extractCompoundRecFromExtra (\(ST.CompoundRec
                    ">-chest"
                    (G.IntLit 1)
                    (ST.CompoundRec
                        ">-chest"
                        (G.IntLit 2)
                        (ST.Simple "humanity"))) -> True)
        it "allows to declare variables of recursive type `<n>-chest of type <n>-miracle" $
            testVoid "<1>-chest of type <2>-miracle" varEntry{ST.entryType = Just ">-chest"}
                U.extractCompoundRecFromExtra (\(ST.CompoundRec
                    ">-chest"
                    (G.IntLit 1)
                    (ST.Compound
                        ">-miracle"
                        (G.IntLit 2))) -> True)
        it "allows to declare variables of recursive type `armor of type sign" $
            testVoid "armor of type sign" varEntry{ST.entryType = Just "armor",  ST.scope = 1}
                U.extractRecursiveFromExtra (\(ST.Recursive
                    "armor"
                    (ST.Simple
                        "sign")) -> True)
        it "allows to declare variables of recursive type `armor of type <n>-chest of type sign" $
            testVoid "armor of type <1>-chest of type sign" varEntry{ST.entryType = Just "armor",  ST.scope = 1}
                U.extractRecursiveFromExtra (\(ST.Recursive
                    "armor"
                    (ST.CompoundRec
                        ">-chest"
                        (G.IntLit 1)
                        (ST.Simple "sign"))) -> True)
        it "allows declare variables of recursive type `armor of type armor of type sign" $
            testVoid "armor of type armor of type sign" varEntry{ST.entryType = Just "armor",  ST.scope = 1}
                U.extractRecursiveFromExtra (\(ST.Recursive
                    "armor"
                    (ST.Recursive
                        "armor"
                        (ST.Simple "sign"))) -> True)
        it "allows to declare 2 or more variables" $ do
            let p = "hello ashen one \

            \ traveling somewhere \

            \ with \
            \ var x of type sign, \
            \ var y of type humanity \
            \ in your inventory \

            \ go back \
            \ you died \

            \ farewell ashen one"
            (_, (dict, _, _), _) <- U.extractSymTable p

            U.testEntry dict varEntry{ST.entryType = Just "sign"} U.extractSimpleFromExtra
                (\(ST.Simple "sign") -> True)
            U.testEntry dict varEntry{ST.entryType = Just "humanity", ST.name = "y"}
                U.extractSimpleFromExtra (\(ST.Simple "humanity") -> True)
    describe "Checks for variable redeclarations and scope chain" $ do
        it "allows to declare the same variable name in a deeper level" $ do
            let p = "hello ashen one \

            \ traveling somewhere \

            \ with \
            \   var x of type humanity \
            \ in your inventory \


            \ trust your inventory \
            \ lit: \
            \   traveling somewhere \
            \   with \
            \       var x of type sign \
            \   in your inventory \

            \   go back \
            \   you died \
            \ inventory closed \
            \ you died \

            \ farewell ashen one"
            (_, (dict, _, _), _) <- U.extractSymTable p
            U.testEntry dict varEntry
                { ST.entryType = Just "humanity" }
                U.extractSimpleFromExtra (\(ST.Simple "humanity") -> True)
            U.testEntry dict varEntry
                { ST.entryType = Just "sign", ST.scope = 2 }
                U.extractSimpleFromExtra (\(ST.Simple "sign") -> True)
        it "rejects to declare the same variable name in the same scope" $ do
            let p = "hello ashen one\n \

            \ traveling somewhere\n \

            \ with\n \
            \   var x of type humanity,\n \
            \   var x of type sign\n \
            \ in your inventory\n \


            \ trust your inventory \
            \ lit: \
            \   traveling somewhere \
            \   with \
            \       var x of type sign \
            \   in your inventory \

            \   go back \
            \   you died \
            \ inventory closed \
            \ you died \

            \ farewell ashen one"
            (_, (dict, _, _), errors) <- U.extractSymTable p
            errors `shouldNotSatisfy` null
            let ST.SemanticError _ (L.Token _ (Just varName) pn) = head errors
            varName `shouldBe` "x"
            L.row pn `shouldBe` 5
            L.col pn `shouldBe` 9
            U.testEntry dict varEntry
                { ST.entryType = Just "humanity" }
                U.extractSimpleFromExtra (\(ST.Simple "humanity") -> True)
            U.testEntry dict varEntry
                { ST.entryType = Just "sign", ST.scope = 2 }
                U.extractSimpleFromExtra (\(ST.Simple "sign") -> True)
    describe "Checks that variable is declared before it is already used" $ do
        it "Allows to use already declared variables" $ do
            let p = "hello ashen one \

            \ traveling somewhere \

            \ with \
            \   var x of type humanity \
            \ in your inventory \
            \ with orange saponite say x \
            \ you died \

            \ farewell ashen one"
            errors <- U.extractErrors p
            errors `shouldSatisfy` null
        it "Allows to use already declared variables that were declared in another scope in the same scope chain" $ do
            let p = "hello ashen one\n\

            \ traveling somewhere\n\

            \ with \
            \   var x of type humanity\n\
            \ in your inventory\n\

            \while the lit covenant is active:\n\
            \traveling somewhere\n\
            \   with orange saponite say x\n\
            \you died\n\
            \covenant left\n\
            \ you died\n\

            \ farewell ashen one\n"
            errors <- U.extractErrors p
            errors `shouldSatisfy` null
        it "Allows to use already declared variables on assignments" $ do
            let p = "hello ashen one \

            \ traveling somewhere \

            \ with \
            \   var x of type humanity \
            \ in your inventory \
            \   x <<= 1 \
            \ you died \

            \ farewell ashen one"
            errors <- U.extractErrors p
            errors `shouldSatisfy` null
        it "Rejects to use not declared variables on assignments" $ do
            let p = "hello ashen one\n\

            \traveling somewhere\n\

            \with\n\
            \   var x of type humanity\n\
            \in your inventory \n\
            \   y <<= 1\n\
            \you died\n\

            \farewell ashen one"
            errors <- U.extractErrors p
            errors `shouldNotSatisfy` null
            let ST.SemanticError _ (L.Token _ (Just varName) pn) = head errors
            varName `shouldBe` "y"
            L.row pn `shouldBe` 6
            L.col pn `shouldBe` 4
        it "Allows to use already declared variables on IO input" $ do
            let p = "hello ashen one \

            \ traveling somewhere \

            \ with \
            \   var x of type humanity \
            \ in your inventory \
            \   transpose into x \
            \ you died \

            \ farewell ashen one"
            errors <- U.extractErrors p
            errors `shouldSatisfy` null
        it "Rejects to use not declared variables on IO input" $ do
            let p = "hello ashen one\n\

            \traveling somewhere\n\

            \with\n\
            \   var x of type humanity\n\
            \in your inventory \n\
            \   transpose into y\n\
            \you died\n\

            \farewell ashen one"
            errors <- U.extractErrors p
            errors `shouldNotSatisfy` null
            let ST.SemanticError _ (L.Token _ (Just varName) pn) = head errors
            varName `shouldBe` "y"
            L.row pn `shouldBe` 6
            L.col pn `shouldBe` 19
        it "Allows to use already declared variables on switch statements" $ do
            let p = "hello ashen one \

            \ traveling somewhere \

            \with \
            \   var x of type humanity \
            \in your inventory \
            \   enter dungeon with x:\
            \      undiscovered:\
            \       traveling somewhere\
            \           go back\
            \       you died\
            \   dungeon exited \
            \ you died \

            \ farewell ashen one"
            errors <- U.extractErrors p
            errors `shouldSatisfy` null
        it "Rejects to use not declared variables on switch statements" $ do
            let p = "hello ashen one\n\

            \traveling somewhere\n\

            \with\n\
            \   var x of type humanity\n\
            \in your inventory \n\
            \   enter dungeon with y:\n\
            \      undiscovered:\
            \       traveling somewhere\
            \           go back\
            \       you died\
            \   dungeon exited \
            \you died\n\

            \farewell ashen one"
            errors <- U.extractErrors p
            errors `shouldNotSatisfy` null
            let ST.SemanticError _ (L.Token _ (Just varName) pn) = head errors
            varName `shouldBe` "y"
            L.row pn `shouldBe` 6
            L.col pn `shouldBe` 23


        it "Rejects to use not declared variables in any scope" $ do
            let p = "hello ashen one\n\

            \ traveling somewhere\n\

            \ with\n\
            \   var x of type humanity\n\
            \ in your inventory\n\
            \   with orange saponite say y\n\
            \ you died \

            \ farewell ashen one"
            errors <- U.extractErrors p
            errors `shouldNotSatisfy` null
            let ST.SemanticError _ (L.Token _ (Just varName) pn) = head errors
            varName `shouldBe` "y"
            L.row pn `shouldBe` 6
            L.col pn `shouldBe` 29
