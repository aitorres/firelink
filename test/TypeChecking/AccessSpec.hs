module AccessSpec where

import           Test.Hspec
import qualified TestUtils  as U

baseProgram :: String -> String -> String -> String
baseProgram t a e = "hello ashen one\n\

\traveling somewhere \n\
\with\n\
\   var x of type " ++ t ++ "\n\
\in your inventory\n\
\   x~>" ++ a ++ " <<= " ++ e ++ "\n\
\you died\n\

\farewell ashen one"

baseProgramWithAliases :: String -> String -> String -> String -> String
baseProgramWithAliases aliases t a e = "hello ashen one\n\

\requiring help of \n\
\" ++ aliases ++ "\n\
\help received\n\

\traveling somewhere \n\
\with\n\
\   var x of type " ++ t ++ "\n\
\in your inventory\n\
\   x~>" ++ a ++ " <<= " ++ e ++ "\n\
\you died\n\

\farewell ashen one"

spec :: Spec
spec =
  describe "`accesses`" $ do
    it "should accept an access for an explicit union" $
      U.shouldNotError $ baseProgram "link { a of type humanity }" "a" "3"

    it "should accept an access for an explicit union with multiple props" $
      U.shouldNotError $ baseProgram "link { a of type humanity, b of type hollow }" "b" "3.5"

    it "should accept an access for an explicit record" $
      U.shouldNotError $ baseProgram "bezel { a of type humanity }" "a" "3"

    it "should accept an access for an explicit record with multiple props" $
      U.shouldNotError $ baseProgram "bezel { a of type humanity, b of type hollow }" "b" "3.5"

    it "should reject an access for a non-struct type" $
      baseProgram "hollow" "a" "3" `U.shouldErrorOn` ("<<=", 6, 5)

    it "should reject an access for an explicit union with an incorrect prop" $
      baseProgram "link { a of type humanity }" "b" "3" `U.shouldErrorOn` ("<<=", 6, 5)

    it "should reject an access for an explicit union with an incorrect type" $
      baseProgram "link { a of type humanity }" "a" "3.5" `U.shouldErrorOn` ("<<=", 6, 9)

    it "should reject an access for an explicit record with an incorrect prop" $
      baseProgram "bezel { a of type humanity }" "b" "3" `U.shouldErrorOn` ("<<=", 6, 5)

    it "should reject an access for an explicit record with an incorrect type" $
      baseProgram "bezel { a of type humanity }" "a" "3.5" `U.shouldErrorOn` ("<<=", 6, 9)

    it "should accept an access for an aliased union" $
        U.shouldNotError $ baseProgramWithAliases "knight lizy link { a of type humanity }" "lizy" "a" "3"

    it "should accept an access for an aliased record" $
        U.shouldNotError $ baseProgramWithAliases "knight bezy bezel { a of type humanity }" "bezy" "a" "3"

    it "should reject an access for an aliased type other than a struct" $
        baseProgramWithAliases "knight bezy hollow" "bezy" "a" "3" `U.shouldErrorOn` ("<<=", 9, 5)

    it "should accept an access for an indirectly aliased union" $
        U.shouldNotError $ baseProgramWithAliases "knight lizy link { a of type humanity }, knight lizzy lizy" "lizzy" "a" "3"

    it "should accept an access for an indirectly aliased record" $
        U.shouldNotError $ baseProgramWithAliases "knight bezy bezel { a of type humanity }, knight bezzy bezy" "bezzy" "a" "3"

    it "should reject an access for an indirectly aliased type other than a struct" $
        baseProgramWithAliases "knight bezy humanity, knight bezzy bezy" "bezzy" "a" "3" `U.shouldErrorOn` ("<<=", 9, 5)

