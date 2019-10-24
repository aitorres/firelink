module InitialTableSpec where

import Test.Hspec
import qualified Grammar as G
import qualified SymTable as ST
import qualified Lexer as L
import qualified Parser as P
import qualified Control.Monad.RWS as RWS
import Data.Maybe (fromJust)

program = "\
\ hello ashen one \
\ traveling somewhere \
\   go back \
\ you died \
\ farewell ashen one"

spec :: Spec
spec = describe "Initial table" $
    it "Should contain `int`" $ do
        tokens <- L.scanTokens program
        (_, (dict, _, _), _) <- RWS.runRWST (P.parse $ fromJust tokens) () ST.initialState
        let intEntry = head $ ST.findChain "int" dict
        ST.name intEntry `shouldBe` "int"
