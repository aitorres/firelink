module OffsetSpec where

import qualified FireLink.FrontEnd.SymTable   as ST
import           Test.Hspec
import qualified TestUtils  as U

testOffset :: String -> [(String, Int)] -> IO ()
testOffset programFragment testItems = do
    dictionary <- getDictionary $ baseProgram programFragment
    mapM_ (test dictionary) testItems
    where
        baseProgram :: String -> String
        baseProgram s = "hello ashen one\n\

                    \ traveling somewhere \
                    \ with \
                    \" ++ s ++ "\
                    \ in your inventory \
                    \ with orange saponite say @hello world@ \
                    \ you died \
                    \ farewell ashen one"

test :: ST.Dictionary -> (String, Int) -> IO ()
test dictionary (varName, expectedOffset) = do
    let chain = filter (\d -> ST.name d == varName) $ ST.findChain varName dictionary
    let dictEntry = head chain
    let ST.Offset actualOffset = U.extractOffsetFromExtra $ ST.extra dictEntry
    actualOffset `shouldBe` expectedOffset
getDictionary :: String -> IO ST.Dictionary
getDictionary program = do
    ST.SymTable {ST.stDict = dict} <- U.extractDictionary program
    return dict

spec :: Spec
spec =
    describe "Offset calculation" $ do
        it "calculates offset for the first variable in the list" $
            testOffset "var x of type humanity" [("x", 0)]
        it "calculates offset for second variable in the list when first variable's type width is multiple of `wordsize`" $
            testOffset "var x of type humanity, var y of type humanity" [("x", 0), ("y", 4)]
        it "calculates offset for second variable in the list when first variable's type width is not a multiple of `wordSize`" $
            testOffset "var x of type sign, var y of type humanity" [("x", 0), ("y", 4)]
        it "calculates offset for variables with mixed data types" $
            testOffset "\
            \var x1 of type bezel {\
            \   x2 of type humanity,\
            \   x3 of type sign\
            \},\
            \var x4 of type sign" [("x1", 0), ("x2", 0), ("x3", 4), ("x4", 5)]
        it "takes advantage of scopes to achieve a better use of offsets" $ do
            let program = "hello ashen one\n\
            \ traveling somewhere \
            \ with \
            \   var x of type humanity\
            \ in your inventory \
            \ while the lit covenant is active:\
            \   traveling somewhere\
            \   with\
            \       var y of type humanity\
            \   in your inventory\
            \       while the lit covenant is active:\
            \         traveling somewhere\
            \         with\
            \             var w of type humanity\
            \         in your inventory\
            \             go back\
            \         you died \
            \       covenant left\
            \   you died \
            \ covenant left \\ \
            \ while the lit covenant is active:\
            \   traveling somewhere\
            \   with\
            \       var z of type humanity\
            \   in your inventory\
            \       go back\
            \   you died \
            \ covenant left\
            \ you died \

            \ farewell ashen one"
            dictionary <- getDictionary program
            mapM_ (test dictionary) [("x", 0), ("y", 4), ("z", 4), ("w", 8)]
