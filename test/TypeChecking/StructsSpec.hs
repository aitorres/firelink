module StructsSpec where

import Test.Hspec
import qualified TestUtils as U

baseProgram :: String -> String -> String
baseProgram t e = "hello ashen one\n\

\traveling somewhere \n\
\with\n\
\   var x of type " ++ t ++ "\n\
\in your inventory\n\
\   x <<= " ++ e ++ "\n\
\you died\n\

\farewell ashen one"

spec :: Spec
spec =
  describe "`struct-like types`" $ do
    it "should reject invalid type assignment on a record" $
      baseProgram "bezel { a of type humanity }" "3" `U.shouldErrorOn` ("<<=", 6, 6)

    it "should reject invalid type assignment on a union" $
      baseProgram "link { a of type humanity }" "3" `U.shouldErrorOn` ("<<=", 6, 6)

    it "should reject invalid type assignment on a record" $
      baseProgram "bezel { a of type humanity }" "{ a <<= 3.6 }" `U.shouldErrorOn` ("<<=", 6, 6)

    it "should reject invalid type assignment on a union" $
      baseProgram "link { a of type humanity }" "{ a <<= 3.6 }" `U.shouldErrorOn` ("<<=", 6, 6)

    it "should reject invalid name assignment on a record" $
      baseProgram "bezel { a of type humanity }" "{ b <<= 3 }" `U.shouldErrorOn` ("<<=", 6, 6)

    it "should reject invalid name assignment on a union" $
      baseProgram "link { a of type humanity }" "{ b <<= 3 }" `U.shouldErrorOn` ("<<=", 6, 6)

    it "should reject assignments of several properties on a union" $
      baseProgram "link { a of type humanity, b of type hollow }" "{ a <<= 2, b <<= 3.5 }" `U.shouldErrorOn` ("<<=", 6, 6)

    it "should reject assignments of less properties on a record" $
      baseProgram "bezel { a of type humanity, b of type hollow }" "{ a <<= 2 }" `U.shouldErrorOn` ("<<=", 6, 6)

    it "should reject assignments of more properties on a record" $
      baseProgram "bezel { a of type humanity, b of type hollow }" "{ a <<= 2, b <<= 4.5, c <<= 12141 }" `U.shouldErrorOn` ("<<=", 6, 6)


