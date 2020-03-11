module FireLink (
    firelink
) where

import qualified Control.Monad.RWS                   as RWS
import qualified Data.Graph                          as G
import           Data.List                           (groupBy, intercalate)
import qualified Data.Map                            as Map
import           FireLink.BackEnd.BackEndCompiler    (backend)
import           FireLink.BackEnd.CodeGenerator      (TAC (..))
import           FireLink.BackEnd.FlowGraphGenerator (NumberedBlock,
                                                      NumberedBlocks)
import           FireLink.FrontEnd.Errors
import           FireLink.FrontEnd.FrontEndCompiler
import qualified FireLink.FrontEnd.SymTable          as ST
import           FireLink.FrontEnd.Tokens
import           FireLink.Utils
import           System.Directory                    (doesFileExist)
import           System.Environment                  (getArgs)
import           System.Exit                         (exitFailure, exitSuccess)
import           System.FilePath                     (takeExtension)
import           System.IO                           (IOMode (..), hGetContents,
                                                      openFile)
import qualified TACType                             as TAC
import           Text.Printf                         (printf)

prettyPrintSymTable :: ST.SymTable -> IO ()
prettyPrintSymTable ST.SymTable{ST.stDict=dict} = do
    let dictList = Map.toList dict
    mapM_ printKey dictList

printKey :: (String, [ST.DictionaryEntry]) -> IO ()
printKey (name, keys) = do
    printf "Entries for name \"%s\": \n" name
    mapM_ printKey' keys
    putStrLn ""
    where
        printKey' :: ST.DictionaryEntry -> IO ()
        printKey' st = do
            putStrLn ""
            putStr " - "
            print st

groupTokensByRowNumber :: [Either Error Token] -> [[Either Error Token]]
groupTokensByRowNumber = groupBy (\e e' -> getRow e == getRow e')
    where
        getRow e = case e of
                    Left (Error _ pn)         -> row pn
                    Right Token {position=pn} -> row pn

lengthOfLine :: [Either Error Token] -> Int
lengthOfLine tokens = case last tokens of
    Right tk@Token {position=pn} -> column pn + length (show tk)
    Left (Error s pn)            -> column pn + length s

joinTokens :: Int -> [Either Error Token] -> String
joinTokens _ [] = ""
joinTokens c (e:tks) = case e of
    Right tk@Token {capturedString=s, position=pn} ->
        replicate (column pn - c) ' ' ++ show tk ++ joinTokens (column pn + length s) tks
    Left (Error s pn) ->
        replicate (column pn - c) ' ' ++ s ++ joinTokens (column pn + length s) tks

buildRuler :: Int -> String
buildRuler = flip replicate '~'

formatLexError :: (Error, [Either Error Token]) -> String
formatLexError (Error _ Position{row=r, column=c}, tokens) =
    printf "%s%sYOU DIED!!%s Lexical error at line %s%s%d%s, column %s%s%d%s:\n%s\n"
        red bold nocolor
        red bold r nocolor
        red bold c nocolor
        fs
    where
        tokensByRowNumber = groupTokensByRowNumber tokens
        maxSize = foldl max (-1) $ map lengthOfLine tokensByRowNumber
        firstLine = joinTokens 1 (head tokensByRowNumber) ++ "\n"
        restLines = map (joinTokens 1) $ tail tokensByRowNumber
        errorRuler = bold ++ red ++ buildRuler (c-1) ++ "^" ++ buildRuler (maxSize - c) ++ nocolor ++ "\n"
        fs = firstLine ++ errorRuler ++ intercalate "\n" restLines


printLexErrors :: [(Error, [Either Error Token])] -> IO ()
printLexErrors [] = return ()
printLexErrors (errorPair:xs) = do
    putStrLn $ formatLexError errorPair
    printLexErrors xs

groupLexErrorWithTokenContext :: [Error] -> [Token] -> [(Error, [Token])]
groupLexErrorWithTokenContext [] _ = []
groupLexErrorWithTokenContext (err@(Error _ pn):errors) tokens =
    (err, tks) : groupLexErrorWithTokenContext errors tokens
    where
        tail' = dropWhile (\Token {position=pn'} -> row pn /= row pn') tokens
        tks = takeWhile (\Token {position=pn'} -> row pn' - row pn <= 4) tail'

insertLexErrorOnContext :: Error -> [Token] -> [Either Error Token]
insertLexErrorOnContext l [] = [Left l]
insertLexErrorOnContext e@(Error _ pn) (tk@Token {position=pn'} : tks) =
    if (row pn' >= row pn) && (column pn' >= column pn)
    then Left e : Right tk : map Right tks
    else Right tk : insertLexErrorOnContext e tks

joinTokensOnly :: [Token] -> String
joinTokensOnly = joinTokens (-1) . map Right

numberOfRows :: [Token] -> Int
numberOfRows = foldl (\cu Token {position=pn} -> cu `max` row pn) (-1)

maxDigits :: [Token] -> Int
maxDigits = length . show . numberOfRows

printProgram :: [Token] -> IO ()
printProgram tks =
    mapM_ printRowAndLine groupedByRowNumber
    where
        groupedByRowNumber :: [[Token]]
        groupedByRowNumber = groupBy (\Token {position=t1} Token {position=t2} -> row t1 == row t2) tks


        printRowAndLine :: [Token] -> IO ()
        printRowAndLine tks' = do
            let Token {position=pn} = head tks'
            let line = row pn
            let lengthOfRowNumber = length $ show line
            putStr $ show line
            putStr $ replicate (maxDigits tks - lengthOfRowNumber) ' '
            putStr "|"
            putStrLn $ joinTokensOnly tks'

printSemErrors :: [Error] -> [Token] -> IO ()
printSemErrors [] _ = do
    putStrLn "Fix your semantic mistakes, ashen one."
    return ()
printSemErrors (semError:semErrors) tokens = do
    let (Error errMessage p) = semError
    let tks = filter (\Token{position=p'} -> row p' == row p) tokens
    let preContext = filter (\Token{position=p'} -> row p' == row p - 1) tokens
    let postContext = filter (\Token{position=p'} -> row p' == row p + 1) tokens
    let nDigits = 2 + maxDigits tks
    RWS.when (not $ null preContext) $ printProgram preContext
    printProgram tks
    putStr $ buildRuler (nDigits + column p)
    putStrLn "^"
    putStrLn $ bold ++ red ++ "YOU DIED!!" ++ nocolor ++ " " ++ errMessage
    RWS.when (not $ null postContext) $ printProgram postContext
    putStrLn "\n"
    printSemErrors semErrors tokens

raiseCompilerError :: String -> IO ()
raiseCompilerError msg = do
    putStrLn $ bold ++ red ++ "YOU DIED!!" ++ nocolor ++ " Compiler error"
    putStrLn msg
    exitFailure

failByNonExistantFile :: String -> IO ()
failByNonExistantFile f =
    raiseCompilerError $ "Your journey cannot be started from " ++ f ++ ", ashen one, as " ++ bold ++ "the file could not be found" ++ nocolor ++ "."

failByFileExtension :: IO ()
failByFileExtension =
    raiseCompilerError $ "The filename MUST have the " ++ bold ++ ".souls" ++ nocolor ++ " extension, ashen one."

failByEmptyArgs :: IO ()
failByEmptyArgs =
    raiseCompilerError $ "Your journey must be started from a " ++ bold ++ red ++ "<file path>" ++ nocolor ++ ", ashen one."

failByEmptyFile :: IO ()
failByEmptyFile =
    raiseCompilerError $ "Your journey must not be started from an " ++ bold ++ red ++ "empty file" ++ nocolor ++ ", ashen one."

failByUnknownFlag :: IO ()
failByUnknownFlag =
    raiseCompilerError $ "Your journey must not be started from an " ++ bold ++ red ++ "unrecognized flag" ++ nocolor ++ ", ashen one."

handleLexError :: [Error] -> [Token] -> IO ()
handleLexError errors tokens = do
    printLexErrors
        $ map (\(err, tks) -> (err, insertLexErrorOnContext err tks))
        $ groupLexErrorWithTokenContext errors tokens
    putStrLn "Fix your lexical mistakes, ashen one."

handleCompileError :: CompilerError -> [Token] -> IO ()
handleCompileError compileError tokens =
    case ceErrorCategory compileError of
        LexError      -> handleLexError (ceErrors compileError) tokens
        SemanticError -> printSemErrors (ceErrors compileError) tokens

printTacCode :: [TAC] -> IO ()
printTacCode = mapM_ print

printBasicBlocks :: NumberedBlocks -> IO ()
printBasicBlocks = mapM_ printBlock
    where
        printBlock :: NumberedBlock -> IO ()
        printBlock (i, cb) = do
            putStrLn $ bold ++ "Block " ++ show i ++ nocolor
            printTacCode cb

printFlowGraph :: G.Graph -> IO ()
printFlowGraph g = do
    let vs = G.vertices g
    let es = G.edges g
    putStrLn $ bold ++ "Graph (" ++ (show . length) vs ++ " nodes, " ++ (show . length) es ++ " edges)" ++ nocolor
    let groupedEdges = groupBy (\a b -> fst a == fst b) es
    mapM_ printEdges groupedEdges
    where
        printEdges :: [G.Edge] -> IO ()
        printEdges es = do
            let origin = (fst . head) es
            let originName = if origin == entryNode then "ENTRY" else show origin
            let destinies = map snd es
            let destiniesNames = joinWithCommas destinies
            putStrLn $ bold ++ originName ++ nocolor ++ " -> " ++ printDestinies destinies

        printDestinies :: [Int] -> String
        printDestinies destinies = if head destinies == exitNode then bold ++ "EXIT" ++ nocolor else joinWithCommas destinies

        entryNode :: Int
        entryNode = -1

        exitNode :: Int
        exitNode = length (G.vertices g) - 2

compile :: String -> Maybe String -> IO ()
compile program compileFlag = do
    compilerResult <- frontEnd program
    case compilerResult of
        Left e -> uncurry handleCompileError e >> exitFailure
        Right (ast, symTable, tokens) -> do
            (tacCode, basicBlocks, flowGraph, allocatedTac) <- backend ast (ST.stDict symTable)
            case compileFlag of
                Nothing -> do
                    -- Default behavior: print everything
                    putStrLn "\nFireLink: Printing SymTable"
                    prettyPrintSymTable symTable
                    putStrLn "\nFireLink: Printing recognized program"
                    printProgram tokens
                    putStrLn "\nFireLink: Printing Intermediate Representation in Three-Address Code"
                    printTacCode tacCode
                    putStrLn "\nFireLink: Printing basic blocks (numbered, starting at 0)"
                    printBasicBlocks basicBlocks
                    putStrLn "\nFireLink: Printing flow graph (with special nodes ENTRY at -1, EXIT as the last one)"
                    printFlowGraph flowGraph
                    putStrLn "\nFireLink: Printing mockup of naive register allocation"
                    putStrLn allocatedTac
                Just flag ->
                    if flag `elem` ["-f", "--frontend"] then prettyPrintSymTable symTable >> printProgram tokens
                    else if flag `elem` ["-s", "--symtable"] then prettyPrintSymTable symTable
                    else if flag `elem` ["-p", "--program"] then printProgram tokens
                    else if flag `elem` ["-t", "--tac"] then printTacCode tacCode
                    else if flag `elem` ["-b", "--blocks"] then printBasicBlocks basicBlocks
                    else if flag `elem` ["-g", "--graph"] then printFlowGraph flowGraph
                    else failByUnknownFlag
            exitSuccess

firelink :: IO ()
firelink = do
    args <- getArgs
    if null args then failByEmptyArgs
    else do
        let programFile = head args
        let compileFlag = if length args < 2 then Nothing else Just (args !! 1)
        fileExists <- doesFileExist programFile
        if fileExists then
            if takeExtension programFile /= ".souls" then failByFileExtension
            else do
                handle <- openFile programFile ReadMode
                contents <- hGetContents handle
                if null contents then failByEmptyFile
                else compile contents compileFlag
        else failByNonExistantFile programFile
