module PointersSpec where

import Test.Hspec
import qualified TestUtils as U
import qualified SymTable as ST

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
            (_, ST.SymTable {ST.stDict=dict}, _) <- U.extractSymTable p
            U.testEntry dict varEntry U.extractRecursiveFromExtra
                (\(ST.Recursive "arrow" (ST.Simple "sign")) -> True)

    describe "Pointers operations" $ do
        it "allows to request memory for its usage" $
            U.shouldNotError $ sampleProgram "aim a x \\"
        it "rejects to request memory of non-declared variables" $
            U.testError (sampleProgram "aim a nonDeclaredVariable \\")
                "nonDeclaredVariable" 10 6
        it "allows to free memory for its usage" $
            U.shouldNotError $ sampleProgram "recover a x \\"
        it "rejects to free memory of non-declared variables" $
            U.testError (sampleProgram "recover a nonDeclaredVariable \\")
                "nonDeclaredVariable" 10 6
        it "allows to access memory address for its usage" $
            U.shouldNotError $ sampleProgram "throw a x <<= |a| \\"
        it "rejects to access memory address of non-declared variables" $
            U.testError (sampleProgram "throw a nonDeclaredVariable <<= |a| \\")
                "nonDeclaredVariable" 10 6
