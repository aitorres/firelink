{
module Parser (
  parse) where

import qualified Lexer as L
import qualified SymTable as ST
import Data.Maybe
import qualified Grammar as G
import qualified Control.Monad.RWS as RWS
}

%name                                                                     parse
%tokentype                                                              { L.Token }
%error                                                                  { parseErrors }
%monad                                                                  { ST.ParserMonad }

%nonassoc lt lte gt gte size memAccessor
%right arrOpen arrClose

%left accessor
%left eq neq
%left plus minus
%left mult div mod

%left and or

%left colConcat
%left diff
%left union intersect


%left not asciiOf


%token
  programBegin                                                          { L.Token L.TkProgramBegin _ _ }
  programEnd                                                            { L.Token L.TkProgramEnd _ _ }

  aliasListBegin                                                        { L.Token L.TkAliasListBegin _ _ }
  aliasListEnd                                                          { L.Token L.TkAliasListEnd _ _ }
  alias                                                                 { L.Token L.TkAlias _ _ }

  id                                                                    { L.Token L.TkId _ _ }

  ofType                                                                { L.Token L.TkOfType _ _ }

  paramRequest                                                          { L.Token L.TkRequesting _ _ }
  parVal                                                                { L.Token L.TkVal _ _ }
  parRef                                                                { L.Token L.TkRef _ _ }

  bigInt                                                                { L.Token L.TkBigInt _ _ }
  smallInt                                                              { L.Token L.TkSmallInt _ _ }
  float                                                                 { L.Token L.TkFloat _ _ }
  char                                                                  { L.Token L.TkChar _ _ }
  bool                                                                  { L.Token L.TkBool _ _ }
  ltelit                                                                { L.Token L.TkLteLit _ _ }
  string                                                                { L.Token L.TkString _ _ }
  array                                                                 { L.Token L.TkArray _ _ }
  set                                                                   { L.Token L.TkSet _ _ }
  enum                                                                  { L.Token L.TkEnum _ _ }
  unionStruct                                                           { L.Token L.TkUnionStruct _ _ }
  record                                                                { L.Token L.TkRecord _ _ }
  pointer                                                               { L.Token L.TkPointer _ _ }

  intLit                                                                { L.Token L.TkIntLit $$ _ }
  floatLit                                                              { L.Token L.TkFloatLit $$ _ }
  charLit                                                               { L.Token L.TkCharLit $$ _ }
  stringLit                                                             { L.Token L.TkStringLit $$ _ }
  trueLit                                                               { L.Token L.TkLit _ _ }
  falseLit                                                              { L.Token L.TkUnlit _ _ }
  unknownLit                                                            { L.Token L.TkUndiscovered _ _ }
  nullLit                                                               { L.Token L.TkNull _ _ }

  functionBegin                                                         { L.Token L.TkInvocation _ _ }
  functionType                                                          { L.Token L.TkInvocationType _ _ }
  functionEnd                                                           { L.Token L.TkInvocationEnd _ _ }

  procedureBegin                                                        { L.Token L.TkSpell _ _ }
  procedureEnd                                                          { L.Token L.TkSpellEnd _ _ }

  comma                                                                 { L.Token L.TkComma _ _ }
  brOpen                                                                { L.Token L.TkBraceOpen _ _ }
  brClose                                                               { L.Token L.TkBraceClosed _ _ }

  with                                                                  { L.Token L.TkWith _ _ }
  declarend                                                             { L.Token L.TkDeclarationEnd _ _ }

  const                                                                 { L.Token L.TkConst _ _ }
  var                                                                   { L.Token L.TkVar _ _ }
  asig                                                                  { L.Token L.TkAsig _ _ }

  instructionsBegin                                                     { L.Token L.TkInstructionBegin _ _ }
  instructionsEnd                                                       { L.Token L.TkInstructionEnd _ _ }
  seq                                                                   { L.Token L.TkSeq _ _ }

  cast                                                                  { L.Token L.TkCast _ _ }
  offering                                                              { L.Token L.TkOffering _ _ }
  toTheKnight                                                           { L.Token L.TkInvocationParsEnd _ _ }

  summon                                                                { L.Token L.TkSummon _ _ }
  granting                                                              { L.Token L.TkGranting _ _ }
  toTheEstusFlask                                                       { L.Token L.TkSpellParsEnd _ _ }

  return                                                                { L.Token L.TkReturn _ _ }
  returnWith                                                            { L.Token L.TkReturnWith _ _ }

  print                                                                 { L.Token L.TkPrint _ _ }
  read                                                                  { L.Token L.TkRead _ _ }

  whileBegin                                                            { L.Token L.TkWhile _ _ }
  whileEnd                                                              { L.Token L.TkEndWhile _ _ }
  covenantIsActive                                                      { L.Token L.TkCovenantIsActive _ _ }

  ifBegin                                                               { L.Token L.TkIf _ _ }
  ifEnd                                                                 { L.Token L.TkEndIf _ _ }
  colon                                                                 { L.Token L.TkColon _ _ }
  else                                                                  { L.Token L.TkElse _ _ }

  switchBegin                                                           { L.Token L.TkSwitch _ _ }
  switchDefault                                                         { L.Token L.TkSwitchDefault _ _ }
  switchEnd                                                             { L.Token L.TkEndSwitch _ _ }

  forBegin                                                              { L.Token L.TkFor _ _ }
  forEnd                                                                { L.Token L.TkEndFor _ _ }
  souls                                                                 { L.Token L.TkSoul _ _ }
  untilLevel                                                            { L.Token L.TkLevel _ _ }

  forEachBegin                                                          { L.Token L.TkForEach _ _ }
  forEachEnd                                                            { L.Token L.TkEndForEach _ _ }
  withTitaniteFrom                                                      { L.Token L.TkWithTitaniteFrom _ _ }

  parensOpen                                                            { L.Token L.TkParensOpen _ _ }
  parensClosed                                                          { L.Token L.TkParensClosed _ _ }

  plus                                                                  { L.Token L.TkPlus _ _ }
  minus                                                                 { L.Token L.TkMinus _ _ }
  mult                                                                  { L.Token L.TkMult _ _ }
  div                                                                   { L.Token L.TkDiv _ _ }
  mod                                                                   { L.Token L.TkMod _ _ }
  lt                                                                    { L.Token L.TkLt _ _ }
  gt                                                                    { L.Token L.TkGt _ _ }
  lte                                                                   { L.Token L.TkLte _ _ }
  gte                                                                   { L.Token L.TkGte _ _ }
  eq                                                                    { L.Token L.TkEq _ _ }
  neq                                                                   { L.Token L.TkNeq _ _ }
  not                                                                   { L.Token L.TkNot _ _ }
  and                                                                   { L.Token L.TkAnd _ _ }
  or                                                                    { L.Token L.TkOr _ _ }
  asciiOf                                                               { L.Token L.TkAsciiOf _ _ }
  colConcat                                                             { L.Token L.TkConcat _ _ }
  union                                                                 { L.Token L.TkUnion _ _ }
  intersect                                                             { L.Token L.TkIntersect _ _ }
  diff                                                                  { L.Token L.TkDiff _ _ }
  size                                                                  { L.Token L.TkSize _ _ }

  arrOpen                                                               { L.Token L.TkArrayOpen _ _ }
  arrClose                                                              { L.Token L.TkArrayClose _ _ }
  setOpen                                                               { L.Token L.TkSetOpen _ _ }
  setClose                                                              { L.Token L.TkSetClose _ _ }

  accessor                                                              { L.Token L.TkAccessor _ _ }
  memAccessor                                                           { L.Token L.TkAccessMemory _ _ }

%%

PROGRAM :: { G.Program }
PROGRAM
  : programBegin ALIASES METHODS CODEBLOCK programEnd                   { G.Program $4 }

ALIASES :: { () }
ALIASES
  : aliasListBegin ALIASL aliasListEnd                                  {% addIdsToSymTable $ reverse $2 }
  | {- empty -}                                                         { () }

ALIASL :: { [NameDeclaration] }
ALIASL
  : ALIASL comma ALIAS                                                  { $3:$1 }
  | ALIAS                                                               { [$1] }

ALIAS :: { NameDeclaration }
ALIAS
  : alias ID TYPE                                                       { (ST.Type, $2, $3) }

EXPR :: { G.Expr }
EXPR
  : intLit                                                              { G.IntLit $ (read (fromJust $1) :: Int) }
  | floatLit                                                            { G.FloatLit $ (read (fromJust $1) :: Float) }
  | charLit                                                             { G.CharLit $ head $ fromJust $1 }
  | stringLit                                                           { G.StringLit $ fromJust $1 }
  | trueLit                                                             { G.TrueLit }
  | falseLit                                                            { G.FalseLit }
  | nullLit                                                             { G.NullLit }
  | arrOpen EXPRL arrClose                                              { G.ArrayLit $ reverse $2 }
  | setOpen EXPRL setClose                                              { G.SetLit $ reverse $2 }
  | unknownLit                                                          { G.UndiscoveredLit }
  | parensOpen EXPR parensClosed                                        { $2 }
  | EXPR accessor ID                                                    { G.Access $1 $3 }
  | EXPR arrOpen EXPR arrClose                                          { G.IndexAccess $1 $3 }
  | memAccessor EXPR                                                    { G.MemAccess $2 }
  | minus EXPR                                                          { G.Negative $2 }
  | not EXPR                                                            { G.Not $2 }
  | asciiOf EXPR                                                        { G.AsciiOf $2 }
  | size EXPR                                                           { G.SetSize $2 }
  | EXPR plus EXPR                                                      { G.Add $1 $3 }
  | EXPR minus EXPR                                                     { G.Substract $1 $3 }
  | EXPR mult EXPR                                                      { G.Multiply $1 $3 }
  | EXPR div EXPR                                                       { G.Divide $1 $3 }
  | EXPR mod EXPR                                                       { G.Mod $1 $3 }
  | EXPR lt EXPR                                                        { G.Lt $1 $3 }
  | EXPR gt EXPR                                                        { G.Gt $1 $3 }
  | EXPR lte EXPR                                                       { G.Lte $1 $3 }
  | EXPR gte EXPR                                                       { G.Gte $1 $3 }
  | EXPR eq EXPR                                                        { G.Eq $1 $3 }
  | EXPR neq EXPR                                                       { G.Neq $1 $3 }
  | EXPR and EXPR                                                       { G.And $1 $3 }
  | EXPR or EXPR                                                        { G.Or $1 $3 }
  | EXPR colConcat EXPR                                                 { G.ColConcat $1 $3 }
  | EXPR union EXPR                                                     { G.SetUnion $1 $3 }
  | EXPR intersect EXPR                                                 { G.SetIntersect $1 $3 }
  | EXPR diff EXPR                                                      { G.SetDiff $1 $3 }
  | FUNCALL                                                             { G.EvalFunc (fst $1) (snd $1) }
  | ID                                                                  {% do
                                                                            checkIdAvailability $1
                                                                            return $ G.IdExpr $1 }

EXPRL :: { G.Exprs }
EXPRL
  : {- empty -}                                                         { [] }
  | EXPRLNOTEMPTY                                                       { $1 }

EXPRLNOTEMPTY :: { G.Exprs }
EXPRLNOTEMPTY
  : EXPR                                                                { [$1] }
  | EXPRLNOTEMPTY comma EXPR                                           { $3:$1 }

METHODS :: { [Int] }
METHODS
  : {- empty -}                                                         { [] }
  | METHODL                                                             { [] }

METHODL :: { [Int] }
METHODL
  : METHODL METHOD                                                      { [] }
  | METHOD                                                              { [] }

METHOD :: { [Int] }
METHOD
  : FUNC                                                                { [] }
  | PROC                                                                { [] }

FUNC :: { [Int] }
FUNC
  : functionBegin ID METHODPARS functionType TYPE CODEBLOCK functionEnd { [] }

PROC :: { [Int] }
PROC
  : procedureBegin ID METHODPARS CODEBLOCK procedureEnd                 { [] }

METHODPARS :: { [Int] }
METHODPARS
  : paramRequest PARS                                                   { [] }
  | {- empty -}                                                         { [] }

PARS :: { [Int] }
PARS
  : PARS comma PAR                                                      { [] }
  | PAR                                                                 { [] }

PAR :: { [Int] }
PAR
  : PARTYPE ID ofType TYPE                                              { [] }

PARTYPE :: { [Int] }
PARTYPE
  : parVal                                                              { [] }
  | parRef                                                              { [] }

TYPE :: { G.Type }
TYPE
  : ID                                                                  { let G.Id t = $1 in G.Simple t Nothing }
  | bigInt                                                              { G.Simple $1 Nothing }
  | smallInt                                                            { G.Simple $1 Nothing }
  | float                                                               { G.Simple $1 Nothing }
  | char                                                                { G.Simple $1 Nothing }
  | bool                                                                { G.Simple $1 Nothing }
  | ltelit EXPR array ofType TYPE                                       { G.Compound $3 $5 (Just $2) }
  | ltelit EXPR string                                                  { G.Simple $3 (Just $2) }
  | set ofType TYPE                                                     { G.Compound $1 $3 Nothing }
  | pointer TYPE                                                        { G.Compound $1 $2 Nothing }
  | record brOpen STRUCTITS brClose                                     { G.Record $1 $ reverse $3 }
  | unionStruct brOpen STRUCTITS brClose                                { G.Record $1 $ reverse $3 }

ENUMITS :: { [Int] }
ENUMITS
  : ENUMITS comma ID                                                    { [] }
  | ID                                                                  { [] }

STRUCTITS :: { [RecordItem] }
STRUCTITS
  : STRUCTITS comma STRUCTIT                                            { $3:$1 }
  | STRUCTIT                                                            { [$1] }

STRUCTIT :: { RecordItem }
STRUCTIT
  : ID ofType TYPE                                                      { ($1, $3) }

ID :: { G.Id }
ID
  : id                                                                  { G.Id $1 }

CODEBLOCK :: { G.CodeBlock }
CODEBLOCK
  : instructionsBegin DECLARS INSTRL instructionsEnd                    { G.CodeBlock $3 }
  | instructionsBegin INSTRL instructionsEnd                            { G.CodeBlock $2 }

DECLARS :: { () }
DECLARS
  : with DECLARSL declarend                                             {% addIdsToSymTable $ reverse $2 }

DECLARSL :: { [NameDeclaration] }
DECLARSL
  : DECLARSL comma DECLAR                                               { $3:$1 }
  | DECLAR                                                              { [$1] }

VARTYPE :: { ST.Category }
VARTYPE
  : const                                                               { ST.Constant }
  | var                                                                 { ST.Variable }

DECLAR :: { NameDeclaration }
DECLAR
  : VARTYPE ID ofType TYPE                                              { ($1, $2, $4) }
  | VARTYPE ID ofType TYPE asig EXPR                                    { ($1, $2, $4) }

INSTRL :: { G.Instructions }
INSTRL
  : INSTRL seq INSTR                                                    { $3 : $1 }
  | INSTR                                                               { [$1] }

INSTR :: { G.Instruction }
INSTR
  : ID asig EXPR                                                        { G.InstAsig $1 $3 }
  | cast ID PROCPARS                                                    { G.InstCallProc $2 $3 }
  | FUNCALL                                                             { G.InstCallFunc (fst $1) (snd $1) }
  | return                                                              { G.InstReturn }
  | returnWith EXPR                                                     { G.InstReturnWith $2 }
  | print EXPR                                                          { G.InstPrint $2 }
  | read ID                                                             { G.InstRead $2 }
  | whileBegin EXPR covenantIsActive colon CODEBLOCK whileEnd           { G.InstWhile $2 $5 }
  | ifBegin IFCASES ELSECASE ifEnd                                      { G.InstIf (reverse ($3 : $2)) }
  | ifBegin IFCASES ifEnd                                               { G.InstIf (reverse $2) }
  | switchBegin ID SWITCHCASES DEFAULTCASE switchEnd                    { G.InstSwitch $2 (reverse ($4 : $3)) }
  | switchBegin ID SWITCHCASES switchEnd                                { G.InstSwitch $2 (reverse $3) }
  | forBegin ID with EXPR souls untilLevel EXPR CODEBLOCK forEnd        { G.InstFor $2 $4 $7 $8 }
  | forEachBegin ID withTitaniteFrom ID CODEBLOCK forEachEnd            { G.InstForEach $2 $4 $5 }

FUNCALL :: { (G.Id, G.Params) }
FUNCALL
  : summon ID FUNCPARS                                                  { ($2, $3) }

IFCASES :: { G.IfCases }
IFCASES
  : IFCASES IFCASE                                                      { $2 : $1 }
  | IFCASE                                                              { [$1] }

IFCASE :: { G.IfCase }
  : EXPR colon CODEBLOCK                                                { G.GuardedCase $1 $3 }

ELSECASE :: { G.IfCase }
  : else colon CODEBLOCK                                                { G.ElseCase $3 }

SWITCHCASES :: { G.SwitchCases }
  : SWITCHCASES SWITCHCASE                                              { $2 : $1 }
  | SWITCHCASE                                                          { [$1] }

SWITCHCASE :: { G.SwitchCase }
  : EXPR colon CODEBLOCK                                                { G.Case $1 $3 }

DEFAULTCASE :: { G.SwitchCase }
  : switchDefault colon CODEBLOCK                                       { G.DefaultCase $3 }

FUNCPARS :: { G.Params }
  : granting PARSLIST toTheKnight                                       { reverse $2 }
  | {- empty -}                                                         { [] }

PROCPARS :: { G.Params }
  : offering PARSLIST toTheEstusFlask                                   { reverse $2 }
  | {- empty -}                                                         { [] }

PARSLIST :: { G.Params }
  : PARSLIST comma EXPR                                                 { $3 : $1 }
  | EXPR                                                                { [$1] }

{

type NameDeclaration = (ST.Category, G.Id, G.Type)
type AliasDeclaration = (G.Id, G.Type)
type RecordItem = AliasDeclaration

constructorTokens :: [L.AbstractToken]
constructorTokens =
  [ L.TkString
  ]

extractFieldsForNewScope :: G.Type -> [RecordItem]
extractFieldsForNewScope (G.Record _ s) = s
extractFieldsForNewScope _ = []

parseErrors :: [L.Token] -> ST.ParserMonad a
parseErrors errors =
  let (L.Token abst _ pn) = errors !! 0
      name = show abst
      line = show $ L.row pn
      column = show $ L.col pn
      header = "\x1b[1m\x1b[31mYOU DIED!!\x1b[0m Parser error: "
      endmsg = "\n\nFix your syntax errors, ashen one."
      position = "line \x1b[1m\x1b[31m" ++ line ++ "\x1b[0m, column \x1b[1m\x1b[31m" ++ column ++ "\x1b[0m."
      msg = header ++ "Unexpected token \x1b[1m\x1b[31m" ++ name ++ "\x1b[0m at " ++ position ++ endmsg
  in  fail msg

addIdsToSymTable :: [NameDeclaration] -> ST.ParserMonad ()
addIdsToSymTable ids = do
  currScope <- ST.enterScope
  (dict, _, _) <- RWS.get
  RWS.mapM_ (addIdToSymTable currScope) ids

addIdToSymTable :: ST.Scope -> NameDeclaration -> ST.ParserMonad ()
addIdToSymTable currScope d@(c, (G.Id tk@(L.Token at (Just idName) _)), t) = do
  maybeIdEntry <- ST.dictLookup idName
  maybeTypeEntry <- findTypeOnEntryTable t
  case maybeIdEntry of
    -- The name doesn't exists on the table, we just add it
    Nothing -> insertIdToEntry t
      ST.DictionaryEntry
        { ST.name = idName
        , ST.category = c
        , ST.scope = currScope
        , ST.entryType = ST.name <$> maybeTypeEntry
        , ST.extra = []
        }

    -- The name does exists on the table, we just add it depending on the scope
    Just entry -> do
      let scope = ST.scope entry
      if currScope /= scope
      then insertIdToEntry t ST.DictionaryEntry
        { ST.name = idName
        , ST.category = c
        , ST.scope = currScope
        , ST.entryType = ST.name <$> maybeTypeEntry
        , ST.extra = []
        }
      else RWS.tell $ [ST.SemanticError ("Name " ++ idName ++ " was already declared on this scope") tk]


insertIdToEntry :: G.Type -> ST.DictionaryEntry -> ST.ParserMonad ()
insertIdToEntry t entry = do
  ex <- buildExtraForType t
  ST.addEntry entry{ST.extra = ex}
  -- To add the record params to the dictionary
  case extractFieldsForNewScope t of
    [] -> return ()
    s ->  do
      addIdsToSymTable $ map (\(a, b) -> (ST.RecordItem, a, b)) s
      ST.exitScope

checkIdAvailability :: G.Id -> ST.ParserMonad ()
checkIdAvailability (G.Id tk@(L.Token _ (Just idName) pn)) = do
  maybeEntry <- ST.dictLookup idName
  case maybeEntry of
    Nothing -> do
      RWS.tell $ [ST.SemanticError ("Name " ++ idName ++ " is not available on this scope") tk]
      return ()
    _ -> do
      return ()

findTypeOnEntryTable :: G.Type -> ST.ParserMonad (Maybe ST.DictionaryEntry)

-- For simple data types
findTypeOnEntryTable (G.Simple tk mSize) = do
  maybeEntry <- ST.dictLookup $ ST.tokensToEntryName tk
  case maybeEntry of
    Nothing -> do
      RWS.tell [ST.SemanticError ("Type " ++ (show tk) ++ " not found") tk]
      return maybeEntry
    _ -> return maybeEntry

-- For compound data types, this extracts the constructor
findTypeOnEntryTable (G.Compound tk _ _) = do
  ST.dictLookup $ ST.tokensToEntryName tk

-- For record alike data types (unions and structs), this extracts their constructor
findTypeOnEntryTable (G.Record tk _) = ST.dictLookup $ ST.tokensToEntryName tk


buildExtraForType :: G.Type -> ST.ParserMonad [ST.Extra]

-- For string alike data types
buildExtraForType t@(G.Simple _ maybeSize) = do
  t' <- (ST.name . fromJust) <$> findTypeOnEntryTable t
  return [case maybeSize of
    Just e -> ST.Compound t' e
    Nothing -> ST.Simple t']

-- For array alike data types
buildExtraForType t@(G.Compound _ tt@(G.Simple _ _) maybeExpr) = do
  maybeTypeEntry <- findTypeOnEntryTable tt
  constructor <- (ST.name . fromJust) <$> findTypeOnEntryTable t -- This call should never fail
  case maybeTypeEntry of
    Just t -> do
      extras <- buildExtraForType tt
      let newExtra = if null extras then (ST.Simple $ ST.name t) else (head extras)
      return (case maybeExpr of
        Just e -> [ST.CompoundRec constructor e newExtra]
        Nothing -> [ST.Recursive constructor newExtra])

buildExtraForType t@(G.Compound _ tt@(G.Compound _ _ _) maybeExpr) = do
  extra' <- head <$> buildExtraForType tt
  constructor <- (ST.name . fromJust) <$> findTypeOnEntryTable t -- This call should never fail
  return $ case maybeExpr of
    Just e -> [ST.CompoundRec constructor e extra']
    Nothing -> [ST.Recursive constructor extra']

buildExtraForType t@(G.Record tk _) = do
  (_, _, currScope) <- RWS.get
  return [ST.RecordFields $ currScope + 1]
-- For anything else
buildExtraForType _ = return []
}
