module WidthSpec where

import qualified FireLink.FrontEnd.SymTable   as ST
import           Test.Hspec
import qualified TestUtils  as U

testWidth :: String -> Int -> IO ()
testWidth programFragment expectedWidth = do
    dictionary <- getDictionary $ baseProgram programFragment
    let chain = filter (\d -> ST.name d == varName) $ ST.findChain varName dictionary
    let ST.Width actualWidth = U.extractWidthFromExtra $ ST.extra $ head chain
    actualWidth `shouldBe` expectedWidth
    where
        varName :: String
        varName = "x"
        getDictionary :: String -> IO ST.Dictionary
        getDictionary program = do
            ST.SymTable {ST.stDict = dict} <- U.extractDictionary program
            return dict
        baseProgram :: String -> String
        baseProgram s = "hello ashen one\n\
                    \ requiring help of \n\
                    \   knight x " ++ s ++ "\
                    \ help received \

                    \ traveling somewhere \
                    \ with orange saponite say @hello world@ \
                    \ you died \
                    \ farewell ashen one"

spec :: Spec
spec = do
    describe "Width calculation for simple data types" $ do
        it "calculates width for the humanity" $
            testWidth "humanity" 4
        it "calculates width for the sign" $
            testWidth "sign" 1
        it "calculates width for the bonfire" $
            testWidth "bonfire" 1
        it "calculates width for the small-humanity" $
            testWidth "small humanity" 2
        it "calculates width for the floats" $
            testWidth "hollow" 8
    describe "Width calculation for composite data types" $ do
        it "calculates width for chests" $
            testWidth "<4>-chest of type humanity" 4
        it "calculates width for chests of chests" $
            testWidth "<4>-chest of type <4>-chest of type humanity" 4
        it "calculates width for sets" $
            testWidth "armor of type humanity" 4
        it "calculates width for sets of sets" $
            testWidth "armor of type armor of type humanity" 4
        it "calculates width for strings" $
            testWidth "<4>-miracle" 4
        it "calculates width for pointers" $
            testWidth "arrow to hollow" 4
    describe "Width calculation for record data types" $ do
        it "calculate width of just 1-attribute record" $
            testWidth "bezel { x of type humanity }" 4
        it "calculate width of 2 attribute records" $
            testWidth "bezel { x of type humanity, y of type humanity }" 4
        it "tries to pack attributes as much as it can without breaking words" $
            testWidth "bezel { x of type humanity, y of type sign }" 5
        it "go to next multiple of 4 if a new attribute is broken" $ do
            testWidth "bezel { y of type sign, x of type humanity }" 8
            testWidth "bezel { y of type sign, y of type bonfire, x of type humanity }" 8
