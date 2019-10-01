{
module Lexer (
    alexMonadScan, scanTokens, getAbstractToken,
    AbstractToken (..), Token (..), AlexUserState(..)
    ) where
import Text.Printf (printf)
import Data.List.Split (splitOn)
import Data.List (intercalate)
}
%wrapper "monadUserState"

-- macros for sets and regex
$digits = [0-9]
$characters = [^\\\|]
@ids = [a-z][A-Za-z0-9_]*
@scapedchars = \\[nt\\\|]
@strings = \@([$characters # \@] | "\@" | @scapedchars)*\@
@comments = \-\- $printable+
tokens :-
    $white+         ;
    -- reserved keywords --
    -- Program structure
    @comments           { makeToken TkComment }
    hello\ ashen\ one   { makeToken TkProgramBegin }
    farewell\ ashen\ one { makeToken TkProgramEnd }
    traveling\ somewhere { makeToken TkInstructionBegin }
    you\ died             { makeToken TkInstructionEnd }
    with                { makeToken TkWith }
    in\ your\ inventory { makeToken TkInYourInventory }
    transpose\ into      { makeToken TkRead }
    with\ orange\ saponite\ say { makeToken TkPrint }

    -- Functions and procedures
    spell               { makeToken TkSpell }
    ashen\ estus\ flask\ consumed { makeToken TkSpellEnd }
    invocation          { makeToken TkInvocation }
    with\ skill\ of\ type { makeToken TkInvocationType }
    requesting          { makeToken TkRequesting }
    val                 { makeToken TkVal }
    ref                 { makeToken TkRef }
    summon              { makeToken TkSummon }
    granting            { makeToken TkGranting }
    cast                { makeToken TkCast }
    offering            { makeToken TkOffering }
    go\ back            { makeToken TkReturn }
    go\ back\ with      { makeToken TkReturnWith }
    after\ this\ return\ to\ your\ world { makeToken TkInvocationEnd }

    -- classic conditional
    trust\ your\ inventory { makeToken TkIf }
    liar\!                  { makeToken TkElse }
    inventory\ closed       { makeToken TkEndIf }

    ascii_of            { makeToken TkAsciiOf }

    -- switch statements
    enter\ dungeon\ with { makeToken TkSwitch }
    dungeon\ exited         { makeToken TkEndSwitch }
    empty\ dungeon           { makeToken TkSwitchDefault }

    -- on-structure looping
    repairing               { makeToken TkRepairing }
    with\ titanite\ from      { makeToken TkWithTitaniteFrom }
    weaponry\ repaired          { makeToken TkEndRepairing }

    -- iterable looping
    upgrading           { makeToken TkUpgrading }
    max\ level\ reached { makeToken TKEndUpgrading }
    souls?              { makeToken TkSoul }
    until\ level        { makeToken TkLevel }

    -- conditionate looping
    while\ the           { makeToken TkWhile }
    covenant\ is\ active { makeToken TkCovenantIsActive }
    covenant\ left       { makeToken TkEndWhile }
    -- Types
    humanity            { makeToken TkBigHumanity}
    small\ humanity     { makeToken TkSmallHumanity}
    big\ humanity       { makeToken TkBigHumanity}

    bonfire             { makeToken TkBonfire }
    lit                 { makeToken TkLit }
    unlit               { makeToken TkUnlit }
    undiscovered        { makeToken TkUndiscovered }

    hollow              { makeToken TkHollow }

    sign                { makeToken TkSign }

    \>\-miracle         { makeToken TkMiracle }

    \>\-chest           { makeToken TkChest }
    \<\$                { makeToken TkChestOpen }
    \$\>                { makeToken TkChestClose }

    armor               { makeToken TkArmor }
    \{\$                { makeToken TkArmorOpen }
    \$\}                { makeToken TkArmorClose }
    union               { makeToken TkUnion }
    intersect           { makeToken TkIntersect }
    diff                { makeToken TkDiff }
    size                { makeToken TkSize }

    titanite            { makeToken TkTitanite }
    \~\>                { makeToken TkAccessor }

    bezel               { makeToken TkBezel }

    link                { makeToken TkLink }

    abyss               { makeToken TkAbyss }
    arrow\ to           { makeToken TkArrowTo }
    aim\ a              { makeToken TkAimA }
    throw\ a            { makeToken TkThrowA }
    recover\ a          { makeToken TkRecoverA }

    knight              { makeToken TkKnight }
    requiring\ help\ of { makeToken TkAliasesListBegin }
    help\ received      { makeToken TkAliasesListEnd }

    -- Literals
    $digits+            { makeToken TkIntLit }
    $digits+\.$digits+  { makeToken TkFloatLit }
    @strings            { makeToken TkString }

    -- Special characters
    \,                  { makeToken TkComma }
    \\                  { makeToken TkSeq }
    \:                   { makeToken TkColon }
    \|$characters\|        { makeToken TkChar }
    \|@scapedchars\|        { makeToken TkChar }

    -- Operators
    \<\<=                 { makeToken TkAsig }
    \+                  { makeToken TkPlus }
    \-                  { makeToken TkMinus }
    \*                  { makeToken TkMult }
    \/                  { makeToken TkDiv }
    \%                   { makeToken TkMod }
    lt                  { makeToken TkLt }
    gt                  { makeToken TkGt }
    lte                  { makeToken TkLte }
    gte                  { makeToken TkGte }
    eq                  { makeToken TkEq }
    neq                  { makeToken TkNeq }
    not                 { makeToken TkNot }
    and                 { makeToken TkAnd }
    or                 { makeToken TkOr }
    \>\-\<              { makeToken TkConcat }
    \<                  { makeToken TkLteLit }
    \{                  { makeToken TkBraceOpen }
    \}                  { makeToken TkBraceClosed }

    const               { makeToken TkConst }
    var                 { makeToken TkVar }
    of\ type            { makeToken TkOfType }
    @ids                { makeToken TkId }
    .                   { throwLexError }

{

payloadRequiredTokens :: [AbstractToken]
payloadRequiredTokens = [
    ]

addPayload :: AbstractToken -> String -> Maybe String
addPayload aToken payload
        | aToken `elem` payloadRequiredTokens = Just payload
        | otherwise = Nothing

makeToken :: AbstractToken -> AlexAction AlexUserState
makeToken token (alexPosn, _, _, str) len = do
    addTokenToState $ Token token (addPayload token $ take len str) alexPosn
    alexMonadScan

throwLexError :: AlexAction AlexUserState
throwLexError alexInput int = do
    addErrorToState $ LexError alexInput
    alexMonadScan

    -- The token type:
data AbstractToken = TkId | TkConst | TkVar | TkOfType | TkAsig
    -------------------
    ------ Types ------
    -------------------
    -- Integers
    | TkBigHumanity | TkSmallHumanity | TkIntLit
    -- Tri-booleans
    | TkBonfire | TkLit | TkUnlit | TkUndiscovered
    -- Double precission
    | TkHollow | TkFloatLit
    -- Character
    | TkSign | TkChar | TkAsciiOf
    --- Collections
    -- Strings
    | TkMiracle | TkLteLit
    | TkString -- Strings
    -- Arrays
    | TkChest | TkChestOpen | TkChestClose | TkSize
    -- Sets
    | TkArmor | TkArmorOpen | TkArmorClose | TkUnion | TkIntersect | TkDiff
    -- Enums
    | TkTitanite | TkBraceOpen | TkBraceClosed | TkComma | TkAccessor
    -- Records (C-like structs)
    | TkBezel
    -- Unions
    | TkLink
    -- Null, pointer stuff
    | TkAbyss | TkArrowTo | TkAimA | TkThrowA | TkRecoverA
    -- Type Aliases
    | TkKnight | TkAliasesListBegin | TkAliasesListEnd

    ------------------
    -- Instructions --
    ------------------
    -- Program basic structure tokens
    | TkComment
    | TkProgramBegin -- hello ashen one
    | TkProgramEnd -- farewell ashen one
    -- Declarations
    | TkInYourInventory
    -- Instructions
    | TkInstructionBegin -- traveling somewhere
    | TkInstructionEnd -- you died
    | TkSeq -- \
    -- Functions
    | TkInvocation | TkRequesting
    | TkInvocationType -- with skill of type
    | TkInvocationEnd -- after this return to your world
    | TkVal | TkRef | TkReturn | TkSummon | TkGranting
    -- Procedures
    | TkSpell
    | TkSpellEnd -- ashen estus flask consumed
    | TkCast
    | TkOffering
    | TkReturnWith
    -- Basic I/O
    | TkPrint -- with orange saponite say
    | TkRead -- transpose into
    -- Basic selection
    | TkIf -- trust your inventory
    | TkColon
    | TkElse -- liar!
    | TkEndIf -- inventory closed
    -- Switch selection
    | TkSwitch -- enter dungeon with <id>
    | TkSwitchDefault
    | TkEndSwitch -- dungeon exited
    -- Finite iterations
    | TkUpgrading -- for without iterable
    | TkWith -- with
    | TkSoul -- soul[s]
    | TkLevel -- until level
    | TKEndUpgrading -- max level reached
    | TkRepairing -- for with iterable
    | TkWithTitaniteFrom -- with titanite from
    | TkEndRepairing -- weaponry repaired
    -- Conditional iterations
    | TkWhile
    | TkCovenantIsActive -- covenant is active
    | TkEndWhile -- covenant left

    -------------------
    -- Common tokens --
    -------------------
    -- Binary operators
    | TkPlus | TkMinus | TkMult | TkDiv | TkMod | TkLt | TkGt | TkLte
    | TkGte | TkEq | TkNeq | TkAnd | TkOr | TkConcat
    -- Unary operators
    | TkNot
    deriving (Eq, Show)

data Token = Token AbstractToken -- Token perse
                (Maybe String) -- Extra info (useful on literals, ids, etc)
                AlexPosn -- To get file context
    deriving (Eq, Show)

type Tokens = [Token]

data LexError = LexError AlexInput
    deriving Show

type LexErrors = [LexError]

-- This isn't on the documentation
alexEOF :: Alex AlexUserState
alexEOF = getUserState

data AlexUserState = LexFailure [LexError]
    | LexSuccess [Token]
    deriving (Show)

alexInitUserState :: AlexUserState
alexInitUserState = LexSuccess []

getUserState :: Alex AlexUserState
getUserState = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, ust)

addTokenToState :: Token -> Alex ()
addTokenToState token = Alex $ \s@AlexState{alex_ust=ust}
    -> Right (s{
        alex_ust = (case ust of
                        LexSuccess tokens -> LexSuccess (token:tokens)
                        _ -> ust)
    }, ())

addErrorToState :: LexError -> Alex ()
addErrorToState lexError = Alex $ \s@AlexState{alex_ust=ust}
    -> Right (s{
        alex_ust = (case ust of
                        LexFailure errors -> LexFailure (lexError:errors)
                        _ -> LexFailure [lexError])
    }, ())

formatLexError :: String -> LexError -> String
formatLexError fullStr (LexError (AlexPn offset r c, _, _, s)) =
    printf "\x1b[1m!!\x1b[0m Lexical error at line \x1b[1m\x1b[31m%d\x1b[0m, column \x1b[1m\x1b[31m%d\x1b[0m:\n%s\n" r c fs
    where
        allLines = splitOn "\n" fullStr
        maxSize = foldl max (-1) $ map length allLines
        buildRuler = flip replicate '~'
        rule = buildRuler maxSize ++ "\n"
        relevantLines = drop (r-1) allLines
        firstLine = head relevantLines ++ "\n"
        restLines = take 4 $ tail relevantLines
        errorRuler = "\x1b[1m\x1b[31m" ++ (buildRuler (c-1)) ++ "^" ++ buildRuler (maxSize - c) ++ "\x1b[0m\n"
        fs = firstLine ++ errorRuler ++ (intercalate "\n" restLines)


printLexErrors :: String -> [LexError] -> IO ()
printLexErrors str [] = return ()
printLexErrors str (error:xs) = do
    putStrLn $ formatLexError str error
    printLexErrors str xs

scanTokens :: String -> IO (Maybe Tokens)
scanTokens str = case runAlex str alexMonadScan of
    Left e -> do
        putStrLn $ "Alex error " ++ show e
        return Nothing
    Right userState -> case userState of
        LexSuccess tokens -> return $ Just $ reverse tokens
        LexFailure errors -> do
            printLexErrors str $ reverse errors
            return Nothing

getAbstractToken :: Token -> AbstractToken
getAbstractToken (Token t _ _) = t
}