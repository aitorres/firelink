{
module Lexer (
    alexMonadScan, scanTokens, filterComments,
    AbstractToken (..), Token (..), AlexUserState(..), AlexPosn (..),
    Tokens, col, row, LexError (..)
    ) where
import Data.List.Extra (replace)

}
%wrapper "monadUserState"

-- macros for sets and regex
$digits = [0-9]
$characters = [^\\\|]
@ids = [a-z][A-Za-z0-9_]*
@scapedchars = \\[nt\\\|]
@strings = \@([$characters # \@] | "\@" | @scapedchars)*\@
@comments = \-\-$printable*
tokens :-
    $white+                               ;
    -- reserved keywords --
    -- Program structure
    @comments                             { makeToken TkComment }
    hello\ ashen\ one                     { makeToken TkProgramBegin }
    farewell\ ashen\ one                  { makeToken TkProgramEnd }
    traveling\ somewhere                  { makeToken TkInstructionBegin }
    you\ died                             { makeToken TkInstructionEnd }
    with                                  { makeToken TkWith }
    in\ your\ inventory                   { makeToken TkDeclarationEnd }
    transpose\ into                       { makeToken TkRead }
    with\ orange\ saponite\ say           { makeToken TkPrint }

    -- Functions and procedures
    spell                                 { makeToken TkSpell }
    to\ the\ estus\ flask                 { makeToken TkSpellParsEnd }
    ashen\ estus\ flask\ consumed         { makeToken TkSpellEnd }
    invocation                            { makeToken TkInvocation }
    with\ skill\ of\ type                 { makeToken TkInvocationType }
    requesting                            { makeToken TkRequesting }
    to\ the\ knight                       { makeToken TkInvocationParsEnd }
    val                                   { makeToken TkVal }
    ref                                   { makeToken TkRef }
    summon                                { makeToken TkSummon }
    granting                              { makeToken TkGranting }
    cast                                  { makeToken TkCast }
    offering                              { makeToken TkOffering }
    go\ back                              { makeToken TkReturn }
    go\ back\ with                        { makeToken TkReturnWith }
    after\ this\ return\ to\ your\ world  { makeToken TkInvocationEnd }

    -- classic conditional
    trust\ your\ inventory                { makeToken TkIf }
    liar\!                                { makeToken TkElse }
    inventory\ closed                     { makeToken TkEndIf }

    ascii_of                              { makeToken TkAsciiOf }

    -- switch statements
    enter\ dungeon\ with                  { makeToken TkSwitch }
    dungeon\ exited                       { makeToken TkEndSwitch }
    empty\ dungeon                        { makeToken TkSwitchDefault }

    -- on-structure looping
    repairing                             { makeToken TkForEach }
    with\ titanite\ from                  { makeToken TkWithTitaniteFrom }
    weaponry\ repaired                    { makeToken TkEndForEach }

    -- iterable looping
    upgrading                             { makeToken TkFor }
    max\ level\ reached                   { makeToken TkEndFor }
    souls?                                { makeToken TkSoul }
    until\ level                          { makeToken TkLevel }

    -- conditionate looping
    while\ the                            { makeToken TkWhile }
    covenant\ is\ active                  { makeToken TkCovenantIsActive }
    covenant\ left                        { makeToken TkEndWhile }
    -- Types
    humanity                              { makeToken TkBigInt }
    small\ humanity                       { makeToken TkSmallInt }
    big\ humanity                         { makeToken TkBigInt }

    bonfire                               { makeToken TkBool }
    lit                                   { makeToken TkLit }
    unlit                                 { makeToken TkUnlit }
    undiscovered                          { makeToken TkUndiscovered }

    hollow                                { makeToken TkFloat }

    sign                                  { makeToken TkChar }

    \>\-miracle                           { makeToken TkString }

    \>\-chest                             { makeToken TkArray }
    \<\$                                  { makeToken TkArrayOpen }
    \$\>                                  { makeToken TkArrayClose }

    armor                                 { makeToken TkSet }
    \{\$                                  { makeToken TkSetOpen }
    \$\}                                  { makeToken TkSetClose }
    union                                 { makeToken TkUnion }
    intersect                             { makeToken TkIntersect }
    diff                                  { makeToken TkDiff }
    size                                  { makeToken TkSize }

    titanite                              { makeToken TkEnum }
    \~\>                                  { makeToken TkAccessor }

    bezel                                 { makeToken TkRecord }

    link                                  { makeToken TkUnionStruct }

    abyss                                 { makeToken TkNull }
    arrow\ to                             { makeToken TkPointer }
    aim\ a                                { makeToken TkRequestMemory }
    throw\ a                              { makeToken TkAccessMemory }
    recover\ a                            { makeToken TkFreeMemory }

    knight                                { makeToken TkAlias }
    requiring\ help\ of                   { makeToken TkAliasListBegin }
    help\ received                        { makeToken TkAliasListEnd }

    -- Literals
    $digits+                              { makeToken TkIntLit }
    $digits+\.$digits+                    { makeToken TkFloatLit }
    @strings                              { makeToken TkStringLit }

    -- Special characters
    \,                                    { makeToken TkComma }
    \\                                    { makeToken TkSeq }
    \:                                    { makeToken TkColon }
    \|$characters\|                       { makeToken TkCharLit }
    \|@scapedchars\|                      { makeToken TkCharLit }

    -- Operators
    \<\<=                                 { makeToken TkAsig }
    \+                                    { makeToken TkPlus }
    \-                                    { makeToken TkMinus }
    \*                                    { makeToken TkMult }
    \/                                    { makeToken TkDiv }
    \%                                    { makeToken TkMod }
    lt                                    { makeToken TkLt }
    gt                                    { makeToken TkGt }
    lte                                   { makeToken TkLte }
    gte                                   { makeToken TkGte }
    eq                                    { makeToken TkEq }
    neq                                   { makeToken TkNeq }
    not                                   { makeToken TkNot }
    and                                   { makeToken TkAnd }
    or                                    { makeToken TkOr }
    \>\-\<                                { makeToken TkConcat }
    \<                                    { makeToken TkLteLit }
    \{                                    { makeToken TkBraceOpen }
    \}                                    { makeToken TkBraceClosed }
    \(                                    { makeToken TkParensOpen }
    \)                                    { makeToken TkParensClosed }

    const                                 { makeToken TkConst }
    var                                   { makeToken TkVar }
    of\ type                              { makeToken TkOfType }
    @ids                                  { makeToken TkId }

    .                                     { throwLexError }

{

payloadRequiredTokens :: [AbstractToken]
payloadRequiredTokens = [TkStringLit, TkIntLit, TkCharLit, TkFloatLit, TkComment, TkId]


makeToken :: AbstractToken -> AlexAction AlexUserState
makeToken token (alexPosn, _, _, str) len = do
    addTokenToState $ Token token (take len str) alexPosn
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
    | TkBigInt | TkSmallInt | TkIntLit
    -- Tri-booleans
    | TkBool | TkLit | TkUnlit | TkUndiscovered
    -- Double precission
    | TkFloat | TkFloatLit
    -- Character
    | TkChar | TkCharLit | TkAsciiOf
    --- Collections
    -- Strings
    | TkString | TkLteLit
    | TkStringLit -- Strings
    -- Arrays
    | TkArray | TkArrayOpen | TkArrayClose | TkSize
    -- Sets
    | TkSet | TkSetOpen | TkSetClose | TkUnion | TkIntersect | TkDiff
    -- Enums
    | TkEnum | TkBraceOpen | TkBraceClosed | TkComma | TkAccessor
    -- Records (C-like structs)
    | TkRecord
    -- Unions
    | TkUnionStruct
    -- Null, pointer stuff
    | TkNull | TkPointer | TkRequestMemory | TkAccessMemory | TkFreeMemory
    -- Type Aliases
    | TkAlias | TkAliasListBegin | TkAliasListEnd

    ------------------
    -- Instructions --
    ------------------
    -- Program basic structure tokens
    | TkComment
    | TkProgramBegin -- hello ashen one
    | TkProgramEnd -- farewell ashen one
    -- Declarations
    | TkDeclarationEnd
    -- Instructions
    | TkInstructionBegin -- traveling somewhere
    | TkInstructionEnd -- you died
    | TkSeq -- \
    -- Functions
    | TkInvocation | TkRequesting
    | TkInvocationType -- with skill of type
    | TkInvocationParsEnd
    | TkInvocationEnd -- after this return to your world
    | TkVal | TkRef | TkReturn | TkSummon | TkGranting
    -- Procedures
    | TkSpell
    | TkSpellEnd -- ashen estus flask consumed
    | TkCast
    | TkOffering
    | TkSpellParsEnd
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
    | TkFor -- for without iterable
    | TkWith -- with
    | TkSoul -- soul[s]
    | TkLevel -- until level
    | TkEndFor -- max level reached
    | TkForEach -- for with iterable
    | TkWithTitaniteFrom -- with titanite from
    | TkEndForEach -- weaponry repaired
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
    -- Parens
    | TkParensOpen | TkParensClosed
    -- Unary operators
    | TkNot
    deriving (Eq)

instance Show AbstractToken where
    show _ = ""
    show TkConst = "const"
    show TkVar = "var"
    show TkOfType = "of type"
    show TkAsig = "<<="
    show TkBigInt = "humanity"
    show TkSmallInt = "small humanity"
    show TkBool = "bonfire"
    show TkLit = "lit"
    show TkUnlit = "unlit"
    show TkUndiscovered = "undiscovered"
    show TkFloat = "hollow"
    show TkChar = "sign"
    show TkAsciiOf = "ascii of"
    show TkString = ">-miracle"
    show TkLteLit = "<"
    show TkArray = ">-chest"
    show TkArrayOpen = "<$"
    show TkArrayClose = "$>"
    show TkSize = "size"
    show TkSet = "armor"
    show TkSetOpen = "{$"
    show TkSetClose = "$}"
    show TkUnion = "union"
    show TkIntersect = "intersect"
    show TkDiff = "diff"
    show TkEnum = "titanite"
    show TkBraceOpen = "{"
    show TkBraceClosed = "}"
    show TkComma = ","
    show TkAccessor = "~>"
    show TkRecord = "bezel"
    show TkUnionStruct = "link"
    show TkNull = "abyss"
    show TkPointer = "pointer to"
    show TkRequestMemory = "aim a"
    show TkAccessMemory = "throw a"
    show TkFreeMemory = "recover a"
    show TkAlias = "knight"
    show TkAliasListBegin = "requiring help of"
    show TkAliasListEnd = "help received"
    show TkProgramBegin = "hello ashen one"
    show TkProgramEnd = "farewell ashen one"
    show TkDeclarationEnd = "in your inventory"
    show TkInstructionBegin = "traveling somewhere"
    show TkInstructionEnd = "you died"

    show TkSeq = "\\"
    show TkInvocation = "invocation"
    show TkRequesting = "requesting"

    show TkInvocationType = "with skill of type"

    show TkInvocationParsEnd = "to the knight"

    show TkInvocationEnd = "after this return to your world"

    show TkVal = "val"
    show TkRef = "ref"
    show TkReturn = "go back"
    show TkSummon = "summon"
    show TkGranting = "granting"
    show TkSpell = "spell"
    show TkSpellEnd = "ashen estus flask consumed"
    show TkCast = "cast"
    show TkOffering = "offering"
    show TkSpellParsEnd = "to the estus flask"
    show TkReturnWith = "go back with"
    show TkPrint = "with orange saponite say"
    show TkRead = "transpose into"
    show TkIf = "trust your inventory"
    show TkColon = ":"
    show TkElse = "liar!"
    show TkEndIf = "inventory closed"
    show TkSwitch = "enter dungeon with"
    show TkSwitchDefault = "empty dungeon"
    show TkEndSwitch = "dungeon exited"
    show TkFor = "upgrading"
    show TkWith = "with"
    show TkSoul = "souls"
    show TkLevel = "until level"
    show TkEndFor = "max level reached"
    show TkForEach = "repairing"
    show TkWithTitaniteFrom = "with titanite from"
    show TkEndForEach = "weaponry repaired"
    show TkWhile = "while the"
    show TkCovenantIsActive = "covenant is active"
    show TkEndWhile = "covenant left"
    show TkPlus = "+"
    show TkMinus = "-"
    show TkMult = "*"
    show TkDiv = "/"
    show TkMod = "%"
    show TkLt = "lt"
    show TkGt = "gt"
    show TkLte = "lte"
    show TkGte = "gte"
    show TkEq = "eq"
    show TkNeq = "neq"
    show TkAnd = "and"
    show TkOr = "or"
    show TkConcat = ">-<"
    show TkParensOpen = "("
    show TkParensClosed = ")"
    show TkNot = "not"

data Token = Token AbstractToken -- Token perse
                String -- The captured token
                AlexPosn -- To get file context
    deriving (Eq)

instance Show Token where
    show (Token aToken s _) = show aToken ++ s

type Tokens = [Token]

data LexError = LexError AlexInput
    deriving Show

type LexErrors = [LexError]

-- This isn't on the documentation
alexEOF :: Alex AlexUserState
alexEOF = getUserState

data AlexUserState = LexerResult LexErrors Tokens
    deriving (Show)

col :: AlexPosn -> Int
col (AlexPn _ _ c) = c

row :: AlexPosn -> Int
row (AlexPn _ l _) = l

alexInitUserState :: AlexUserState
alexInitUserState = LexerResult [] []

getUserState :: Alex AlexUserState
getUserState = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, ust)

addTokenToState :: Token -> Alex ()
addTokenToState token = Alex $ \s@AlexState{alex_ust=ust}
    -> Right (s{
        alex_ust = (
            let LexerResult errors tokens = ust in
                LexerResult errors (token:tokens))
    }, ())

addErrorToState :: LexError -> Alex ()
addErrorToState lexError = Alex $ \s@AlexState{alex_ust=ust}
    -> Right (s{
        alex_ust = (
            let LexerResult errors tokens = ust in
                LexerResult (lexError:errors) tokens)
    }, ())

scanTokens :: String -> (LexErrors, Tokens)
scanTokens str = case runAlex str alexMonadScan of
    Left e -> do
        error $ "Alex error " ++ show e
    Right userState ->
        let LexerResult errors tokens = userState in (
            reverse errors,
            filterComments $ map postProcess $ reverse tokens)

removeFirstAndLast :: [a] -> [a]
removeFirstAndLast = reverse . tail . reverse . tail

postProcess :: Token -> Token
postProcess (Token TkCharLit s p) = Token TkCharLit (f s) p
    where
        f s = if head a == '\\' then mapEscaped $ last a else a
        a = removeFirstAndLast s
        mapEscaped 'n' = "\n"
        mapEscaped 't' = "\t"
        mapEscaped '\\' = "\\"
        mapEscaped '|' = "\n"
postProcess (Token TkStringLit s p) = Token TkStringLit cleanedString p
    where
        pp = removeFirstAndLast s
        cleanedString = replace "\\@" "@" pp
postProcess a = a

-- getAbstractToken :: Token -> AbstractToken
-- getAbstractToken (Token t _ _) = t

filterComments :: [Token] -> [Token]
filterComments =
    filter (\(Token abst _ _) -> abst /= TkComment)
}
