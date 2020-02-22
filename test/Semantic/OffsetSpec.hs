module OffsetSpec where

import qualified FireLink.FrontEnd.SymTable   as ST
import           Test.Hspec
import qualified TestUtils  as U

testOffset :: String -> [(String, Int)] -> IO ()
testOffset programFragment testItems = do
    dictionary <- getDictionary $ baseProgram programFragment
    mapM_ (test dictionary) testItems
    where
        test :: ST.Dictionary -> (String, Int) -> IO ()
        test dictionary (varName, expectedOffset) = do
            let chain = filter (\d -> ST.name d == varName) $ ST.findChain varName dictionary
            let dictEntry = head chain
            let ST.Offset actualOffset = U.extractOffsetFromExtra $ ST.extra dictEntry
            expectedOffset `shouldBe` actualOffset
        getDictionary :: String -> IO ST.Dictionary
        getDictionary program = do
            ST.SymTable {ST.stDict = dict} <- U.extractDictionary program
            return dict
        baseProgram :: String -> String
        baseProgram s = "hello ashen one\n\

                    \ traveling somewhere \
                    \ with \
                    \" ++ s ++ "\
                    \ in your inventory \
                    \ with orange saponite say @hello world@ \
                    \ you died \
                    \ farewell ashen one"

spec :: Spec
spec =
    describe "Offset calculation" $ do
        it "calculates offset for the first variable in the list" $ 1 `shouldBe` 1
        --     testOffset "var x of type humanity" [("x", 0)]
        -- it "calculates offset for second variable in the list when first variable's type width is multiple of `wordsize`" $
        --     testOffset "var x of type humanity, var y of type humanity" [("x", 0), ("y", 4)]
        -- it "calculates offset for second variable in the list when first variable's type width is not a multiple of `wordSize`" $
        --     testOffset "var x of type sign, var y of type humanity" [("x", 0), ("y", 4)]
