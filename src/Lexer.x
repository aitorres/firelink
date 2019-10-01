{
module Lexer (
    alexMonadScan, scanTokens,
    Token (..), AlexUserState(..)
    ) where
}
%wrapper "monadUserState"

-- macros for sets and regex
$digits = [0-9]
$characters = [^\\\|]
@ids = [a-z][A-Za-z0-9_]*
@scapedchars = \\[nt\\\|]
@strings = \@([$characters # \@] | "\@" | @scapedchars)+\@
tokens :-
    $white+         ;
    -- reserved keywords
    hello\ ashen\ one   { makeToken TkProgramBegin }
    farewell\ ashen\ one { makeToken TkProgramEnd }
    traveling\ somewhere { makeToken TkInstructionBegin }
    you\ died             { makeToken TkInstructionEnd }
    with                { makeToken TkWith }
    in\ your\ inventory { makeToken TkInYourInventory }
    transpose\ into      { makeToken TkRead }
    upgrading           { makeToken TkUpgrading }
    max\ level\ reached { makeToken TKEndUpgrading }
    souls?              { makeToken TkSoul }
    until\ level        { makeToken TkLevel }
    with\ orange\ saponite\ say { makeToken TkPrint }
    spell               { makeToken TkSpell }
    ashen\ estus\ flask\ consumed { makeToken TkSpellEnd }
    invocation          { makeToken TkInvocation }
    requesting          { makeToken TkRequesting }
    val                 { makeToken TkVal }
    ref                 { makeToken TkRef }
    with\ skill\ of\ type { makeToken TkInvocationType }
    trust\ your\ inventory { makeToken TkIf }
    inventory\ closed       { makeToken TkEndIf }
    summon              { makeToken TkSummon }
    granting            { makeToken TkGranting }
    go\ back\ with      { makeToken TkReturn }
    after\ this\ return\ to\ your\ world { makeToken TkInvocationEnd }
    ascii_of            { makeToken TkAsciiOf }

    -- Types
    humanity            { makeToken TkBigHumanity}
    small\ humanity            { makeToken TkBigHumanity}
    big\ humanity            { makeToken TkBigHumanity}

    bonfire             { makeToken TkBonfire }
    lit                 { makeToken TkLit }
    unlit               { makeToken TkUnlit }
    undiscovered        { makeToken TkUndiscovered }

    sign                { makeToken TkSign }

    \>\-miracle         { makeToken TkMiracle }

    -- Literals
    $digits+            { makeToken TkIntLit }
    $digits+\.$digits+  { makeToken TkFloatLit }
    @strings            { makeToken TkString }

    -- Special characters
    \,                  { makeToken TkComma }
    \\                  { makeToken TkSeq }
    :                   { makeToken TkColon }
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
    | TkChestKnown | TkChestUnknown | TkChestOpen | TkChestClose | TkSize
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
    | TkKnight

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
    | TkDefault
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
    deriving (Show)

type Tokens = [Token]

data LexError = LexError AlexInput
    deriving (Show)

type LexErrors = [LexError]

-- This isn't on the documentation
alexEOF :: Alex AlexUserState
alexEOF = getUserState

data AlexUserState = LexFailure LexErrors
    | LexSuccess Tokens

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

printLexErrors :: [LexError] -> IO ()
printLexErrors = mapM_ print -- mapM_ is the monadic version of map

scanTokens :: String -> IO (Maybe Tokens)
scanTokens str = case runAlex str alexMonadScan of
    Left e -> do
        putStrLn $ "Alex error " ++ show e
        return Nothing
    Right userState -> case userState of
        LexSuccess tokens -> return $ Just $ reverse tokens
        LexFailure errors -> do
            printLexErrors $ reverse errors
            return Nothing
}
