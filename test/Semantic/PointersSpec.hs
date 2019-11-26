module PointersSpec where

import Test.Hspec
import qualified Utils as U
import qualified SymTable as ST
import qualified Lexer as L

varEntry :: ST.DictionaryEntry
varEntry = ST.DictionaryEntry
    { ST.scope = 1
    , ST.category = ST.Variable
    , ST.entryType = Just "arrow"
    , ST.name = "x"
    , ST.extra = []
    }

sampleProgram :: String -> String
sampleProgram instr = "hello ashen one\n\

\traveling somewhere\n\
\with\n\
\   var x of type arrow to sign\n\
\in your inventory\n\
\   " ++ instr ++ "\n\
\   with orange saponite say @hello world@\n\
\you died\n\

\farewell ashen one\n"

spec :: Spec
spec = do
    describe "Pointers declarations" $
        it "allows to declare variables with pointers data types" $ do
            let p = sampleProgram ""
            (_, (dict, _, _), _) <- U.extractSymTable p
            U.testEntry dict varEntry U.extractRecursiveFromExtra
                (\(ST.Recursive "arrow" (ST.Simple "sign")) -> True)

    describe "Pointers operations" $ do
        it "allows to request memory for its usage" $
            U.shouldNotError $ sampleProgram "aim a x \\"
        it "rejects to request memory of non-declared variables" $ do
            let p = sampleProgram "aim a nonDeclaredVariable \\"
            (_, _, errors) <- U.extractSymTable p
            errors `shouldNotSatisfy` null
            let ST.SemanticError _ L.Token {L.cleanedString=varName, L.posn=pn} = head errors
            varName `shouldBe` "nonDeclaredVariable"
            L.col pn `shouldBe` 10
            L.row pn `shouldBe` 6
