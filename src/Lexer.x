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

col :: AlexPosn -> Int
col (AlexPn _ _ c) = c

row :: AlexPosn -> Int
row (AlexPn _ l _) = l

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

scanTokens :: String -> Either LexErrors Tokens
scanTokens str = case runAlex str alexMonadScan of
    Left e -> do
        error $ "Alex error " ++ show e
    Right userState -> case userState of
        LexSuccess tokens -> Right $ filterComments $ map postProcess $ reverse tokens
        LexFailure errors -> Left $ reverse errors

removeFirstAndLast :: [a] -> [a]
removeFirstAndLast = reverse . tail . reverse . tail

postProcess :: Token -> Token
postProcess (Token TkCharLit (Just s) p) = Token TkCharLit (Just $ f s) p
    where
        f s = if head a == '\\' then mapEscaped $ last a else a
        a = removeFirstAndLast s
        mapEscaped 'n' = "\n"
        mapEscaped 't' = "\t"
        mapEscaped '\\' = "\\"
        mapEscaped '|' = "\n"
postProcess (Token TkStringLit (Just s) p) = Token TkStringLit (Just cleanedString) p
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
