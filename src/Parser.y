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


%nonassoc size memAccessor

%left ARRCLOSE

%left eq neq
%left plus minus
%left mult div mod
%left NEG

%left and or

%nonassoc lt lte gt gte

%left colConcat
%left diff
%left union intersect


%left asciiOf
%right not
%left accessor
%left arrOpen arrClose


%token
  programBegin                                                          { L.Token {L.aToken=L.TkProgramBegin} }
  programEnd                                                            { L.Token {L.aToken=L.TkProgramEnd} }

  aliasListBegin                                                        { L.Token {L.aToken=L.TkAliasListBegin} }
  aliasListEnd                                                          { L.Token {L.aToken=L.TkAliasListEnd} }
  alias                                                                 { L.Token {L.aToken=L.TkAlias} }

  id                                                                    { L.Token {L.aToken=L.TkId} }

  ofType                                                                { L.Token {L.aToken=L.TkOfType} }

  paramRequest                                                          { L.Token {L.aToken=L.TkRequesting} }
  parVal                                                                { L.Token {L.aToken=L.TkVal} }
  parRef                                                                { L.Token {L.aToken=L.TkRef} }

  bigInt                                                                { L.Token {L.aToken=L.TkBigInt} }
  smallInt                                                              { L.Token {L.aToken=L.TkSmallInt} }
  float                                                                 { L.Token {L.aToken=L.TkFloat} }
  char                                                                  { L.Token {L.aToken=L.TkChar} }
  bool                                                                  { L.Token {L.aToken=L.TkBool} }
  ltelit                                                                { L.Token {L.aToken=L.TkLteLit} }
  string                                                                { L.Token {L.aToken=L.TkString} }
  array                                                                 { L.Token {L.aToken=L.TkArray} }
  set                                                                   { L.Token {L.aToken=L.TkSet} }
  enum                                                                  { L.Token {L.aToken=L.TkEnum} }
  unionStruct                                                           { L.Token {L.aToken=L.TkUnionStruct} }
  record                                                                { L.Token {L.aToken=L.TkRecord} }
  pointer                                                               { L.Token {L.aToken=L.TkPointer} }

  intLit                                                                { L.Token {L.aToken=L.TkIntLit, L.cleanedString=$$} }
  floatLit                                                              { L.Token {L.aToken=L.TkFloatLit, L.cleanedString=$$} }
  charLit                                                               { L.Token {L.aToken=L.TkCharLit, L.cleanedString=$$} }
  stringLit                                                             { L.Token {L.aToken=L.TkStringLit, L.cleanedString=$$} }
  trueLit                                                               { L.Token {L.aToken=L.TkLit} }
  falseLit                                                              { L.Token {L.aToken=L.TkUnlit} }
  unknownLit                                                            { L.Token {L.aToken=L.TkUndiscovered} }
  nullLit                                                               { L.Token {L.aToken=L.TkNull} }

  functionBegin                                                         { L.Token {L.aToken=L.TkInvocation} }
  functionType                                                          { L.Token {L.aToken=L.TkInvocationType} }
  functionEnd                                                           { L.Token {L.aToken=L.TkInvocationEnd} }

  procedureBegin                                                        { L.Token {L.aToken=L.TkSpell} }
  procedureEnd                                                          { L.Token {L.aToken=L.TkSpellEnd} }

  comma                                                                 { L.Token {L.aToken=L.TkComma} }
  brOpen                                                                { L.Token {L.aToken=L.TkBraceOpen} }
  brClose                                                               { L.Token {L.aToken=L.TkBraceClosed} }

  with                                                                  { L.Token {L.aToken=L.TkWith} }
  declarend                                                             { L.Token {L.aToken=L.TkDeclarationEnd} }

  const                                                                 { L.Token {L.aToken=L.TkConst} }
  var                                                                   { L.Token {L.aToken=L.TkVar} }
  asig                                                                  { L.Token {L.aToken=L.TkAsig} }

  instructionsBegin                                                     { L.Token {L.aToken=L.TkInstructionBegin} }
  instructionsEnd                                                       { L.Token {L.aToken=L.TkInstructionEnd} }
  seq                                                                   { L.Token {L.aToken=L.TkSeq} }

  cast                                                                  { L.Token {L.aToken=L.TkCast} }
  offering                                                              { L.Token {L.aToken=L.TkOffering} }
  toTheKnight                                                           { L.Token {L.aToken=L.TkInvocationParsEnd} }

  summon                                                                { L.Token {L.aToken=L.TkSummon} }
  granting                                                              { L.Token {L.aToken=L.TkGranting} }
  toTheEstusFlask                                                       { L.Token {L.aToken=L.TkSpellParsEnd} }

  return                                                                { L.Token {L.aToken=L.TkReturn} }
  returnWith                                                            { L.Token {L.aToken=L.TkReturnWith} }

  print                                                                 { L.Token {L.aToken=L.TkPrint} }
  read                                                                  { L.Token {L.aToken=L.TkRead} }

  whileBegin                                                            { L.Token {L.aToken=L.TkWhile} }
  whileEnd                                                              { L.Token {L.aToken=L.TkEndWhile} }
  covenantIsActive                                                      { L.Token {L.aToken=L.TkCovenantIsActive} }

  ifBegin                                                               { L.Token {L.aToken=L.TkIf} }
  ifEnd                                                                 { L.Token {L.aToken=L.TkEndIf} }
  colon                                                                 { L.Token {L.aToken=L.TkColon} }
  else                                                                  { L.Token {L.aToken=L.TkElse} }

  switchBegin                                                           { L.Token {L.aToken=L.TkSwitch} }
  switchDefault                                                         { L.Token {L.aToken=L.TkSwitchDefault} }
  switchEnd                                                             { L.Token {L.aToken=L.TkEndSwitch} }

  forBegin                                                              { L.Token {L.aToken=L.TkFor} }
  forEnd                                                                { L.Token {L.aToken=L.TkEndFor} }
  souls                                                                 { L.Token {L.aToken=L.TkSoul} }
  untilLevel                                                            { L.Token {L.aToken=L.TkLevel} }

  forEachBegin                                                          { L.Token {L.aToken=L.TkForEach} }
  forEachEnd                                                            { L.Token {L.aToken=L.TkEndForEach} }
  withTitaniteFrom                                                      { L.Token {L.aToken=L.TkWithTitaniteFrom} }

  parensOpen                                                            { L.Token {L.aToken=L.TkParensOpen} }
  parensClosed                                                          { L.Token {L.aToken=L.TkParensClosed} }

  plus                                                                  { L.Token {L.aToken=L.TkPlus} }
  minus                                                                 { L.Token {L.aToken=L.TkMinus} }
  mult                                                                  { L.Token {L.aToken=L.TkMult} }
  div                                                                   { L.Token {L.aToken=L.TkDiv} }
  mod                                                                   { L.Token {L.aToken=L.TkMod} }
  lt                                                                    { L.Token {L.aToken=L.TkLt} }
  gt                                                                    { L.Token {L.aToken=L.TkGt} }
  lte                                                                   { L.Token {L.aToken=L.TkLte} }
  gte                                                                   { L.Token {L.aToken=L.TkGte} }
  eq                                                                    { L.Token {L.aToken=L.TkEq} }
  neq                                                                   { L.Token {L.aToken=L.TkNeq} }
  not                                                                   { L.Token {L.aToken=L.TkNot} }
  and                                                                   { L.Token {L.aToken=L.TkAnd} }
  or                                                                    { L.Token {L.aToken=L.TkOr} }
  asciiOf                                                               { L.Token {L.aToken=L.TkAsciiOf} }
  colConcat                                                             { L.Token {L.aToken=L.TkConcat} }
  union                                                                 { L.Token {L.aToken=L.TkUnion} }
  intersect                                                             { L.Token {L.aToken=L.TkIntersect} }
  diff                                                                  { L.Token {L.aToken=L.TkDiff} }
  size                                                                  { L.Token {L.aToken=L.TkSize} }

  arrOpen                                                               { L.Token {L.aToken=L.TkArrayOpen} }
  arrClose                                                              { L.Token {L.aToken=L.TkArrayClose} }
  setOpen                                                               { L.Token {L.aToken=L.TkSetOpen} }
  setClose                                                              { L.Token {L.aToken=L.TkSetClose} }

  accessor                                                              { L.Token {L.aToken=L.TkAccessor} }
  memAccessor                                                           { L.Token {L.aToken=L.TkAccessMemory} }

%%

PROGRAM :: { G.Program }
PROGRAM
  : programBegin ALIASES METHODS NON_OPENER_CODEBLOCK programEnd        { G.Program $4 }

NON_OPENER_CODEBLOCK :: { G.CodeBlock }
NON_OPENER_CODEBLOCK
  : instructionsBegin DECLARS INSTRL instructionsEnd                     { G.CodeBlock $ reverse $3 }
  | instructionsBegin INSTRL instructionsEnd                             { G.CodeBlock $ reverse $2 }

ALIASES :: { () }
ALIASES
  : aliasListBegin ALIASL aliasListEnd                                  {% do
                                                                            addIdsToSymTable $ reverse $2
                                                                            (dict, _, s) <- RWS.get
                                                                            RWS.put (dict, [1, 0], s) }
  | {- empty -}                                                         { () }


ALIASL :: { [NameDeclaration] }
ALIASL
  : ALIASL comma ALIAS                                                  { $3:$1 }
  | ALIAS                                                               { [$1] }

ALIAS :: { NameDeclaration }
ALIAS
  : alias ID TYPE                                                       { (ST.Type, $2, $3) }

LVALUE :: { G.Expr }
LVALUE
  : ID                                                                  {% do
                                                                            checkIdAvailability $1
                                                                            return $ G.IdExpr $1 }
  | EXPR accessor ID                                                    {% do
                                                                            let ret = G.Access $1 $3
                                                                            checkPropertyAvailability ret
                                                                            return ret }
  | EXPR arrOpen EXPR arrClose                                          { G.IndexAccess $1 $3 }

EXPR :: { G.Expr }
EXPR
  : intLit                                                              { G.IntLit $ (read $1 :: Int) }
  | floatLit                                                            { G.FloatLit $ (read $1 :: Float) }
  | charLit                                                             { G.CharLit $ head $1 }
  | stringLit                                                           { G.StringLit $1 }
  | trueLit                                                             { G.TrueLit }
  | falseLit                                                            { G.FalseLit }
  | nullLit                                                             { G.NullLit }
  | arrOpen EXPRL arrClose                                              { G.ArrayLit $ reverse $2 }
  | setOpen EXPRL setClose                                              { G.SetLit $ reverse $2 }
  | unknownLit                                                          { G.UndiscoveredLit }
  | parensOpen EXPR parensClosed                                        { $2 }
  | memAccessor EXPR                                                    { G.MemAccess $2 }
  | minus EXPR %prec NEG                                                { G.Negative $2 }
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
  | LVALUE                                                              { $1 }

EXPRL :: { G.Exprs }
EXPRL
  : {- empty -}                                                         { [] }
  | EXPRLNOTEMPTY                                                       { $1 }

EXPRLNOTEMPTY :: { G.Exprs }
EXPRLNOTEMPTY
  : EXPR                                                                { [$1] }
  | EXPRLNOTEMPTY comma EXPR                                           { $3:$1 }

METHODS :: { () }
METHODS
  : {- empty -}                                                         { () }
  | METHODL                                                             { () }

METHODL :: { () }
METHODL
  : METHODL METHOD                                                      { () }
  | METHOD                                                              { () }

METHOD :: { () }
METHOD
  : FUNC                                                                {% do
                                                                          (dict, _, s) <- RWS.get
                                                                          RWS.put (dict, [1, 0], s) }
  | PROC                                                                {% do
                                                                          (dict, _, s) <- RWS.get
                                                                          RWS.put (dict, [1, 0], s) }

FUNCPREFIX :: { Maybe (ST.Scope, G.Id) }
FUNCPREFIX
  : functionBegin ID METHODPARS functionType TYPE                       {% addFunction (ST.Function,
                                                                              $2, G.Callable (Just $5) $3) }
FUNC :: { () }
FUNC
  : FUNCPREFIX NON_OPENER_CODEBLOCK functionEnd                         {% case $1 of
                                                                            Nothing -> return ()
                                                                            Just (s, i) -> updateCodeBlockOfFun s i $2 }

PROCPREFIX :: { Maybe (ST.Scope, G.Id) }
PROCPREFIX
  : procedureBegin ID PROCPARSDEC                                       {% addFunction (ST.Procedure,
                                                                              $2, G.Callable Nothing $3) }
PROC :: { () }
PROC
  : PROCPREFIX NON_OPENER_CODEBLOCK procedureEnd                        {% case $1 of
                                                                            Nothing -> return ()
                                                                            Just (s, i) -> updateCodeBlockOfFun s i $2 }

PROCPARSDEC :: { [ArgDeclaration] }
PROCPARSDEC
  : METHODPARS toTheEstusFlask                                          { $1 }
  | {- empty -}                                                         { [] }

METHODPARS :: { [ArgDeclaration] }
METHODPARS
  : paramRequest PARS                                                   { reverse $2 }
  | {- empty -}                                                         { [] }

PARS :: { [ArgDeclaration] }
PARS
  : PARS comma PAR                                                      { $3:$1 }
  | PAR                                                                 { [$1] }

PAR :: { ArgDeclaration }
PAR
  : PARTYPE ID ofType TYPE                                              { ($1, $2, $4) }

PARTYPE :: { G.ArgType }
PARTYPE
  : parVal                                                              { G.Val }
  | parRef                                                              { G.Ref }

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
  : INSTBEGIN DECLARS INSTRL INSTEND                                    { G.CodeBlock $ reverse $3 }
  | INSTBEGIN INSTRL INSTEND                                            { G.CodeBlock $ reverse $2 }

INSTBEGIN :: { () }
INSTBEGIN : instructionsBegin                                           {% ST.enterScope }

INSTEND :: { () }
INSTEND : instructionsEnd                                               {% ST.exitScope }

DECLARS :: { () }
DECLARS
  : with DECLARSL declarend                                             {% addIdsToSymTable (reverse $2) }

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
  : LVALUE asig EXPR                                                    {% do
                                                                          checkConstantReassignment $1
                                                                          return $ G.InstAsig $1 $3 }
  | cast ID PROCPARS                                                    {% do
                                                                          checkIdAvailability $2
                                                                          return $ G.InstCallProc $2 $3 }
  | FUNCALL                                                             { G.InstCallFunc (fst $1) (snd $1) }
  | return                                                              { G.InstReturn }
  | returnWith EXPR                                                     { G.InstReturnWith $2 }
  | print EXPR                                                          { G.InstPrint $2 }
  | read LVALUE                                                         { G.InstRead $2 }
  | whileBegin EXPR covenantIsActive colon CODEBLOCK whileEnd           { G.InstWhile $2 $5 }
  | ifBegin IFCASES ELSECASE ifEnd                                      { G.InstIf (reverse ($3 : $2)) }
  | ifBegin IFCASES ifEnd                                               { G.InstIf (reverse $2) }
  | switchBegin EXPR colon SWITCHCASES DEFAULTCASE switchEnd            { G.InstSwitch $2 (reverse ($5 : $4)) }
  | switchBegin EXPR colon SWITCHCASES switchEnd                        { G.InstSwitch $2 (reverse $4) }
  | forBegin ID with EXPR souls untilLevel EXPR CODEBLOCK forEnd        { G.InstFor $2 $4 $7 $8 }
  | forEachBegin ID withTitaniteFrom EXPR CODEBLOCK forEachEnd          { G.InstForEach $2 $4 $5 }

FUNCALL :: { (G.Id, G.Params) }
FUNCALL
  : summon ID FUNCPARS                                                  {% do
                                                                          checkIdAvailability $2
                                                                          return ($2, $3) }

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
type ArgDeclaration = (G.ArgType, G.Id, G.Type)
type AliasDeclaration = (G.Id, G.Type)
type RecordItem = AliasDeclaration

extractFieldsForNewScope :: G.Type -> Maybe [RecordItem]
extractFieldsForNewScope (G.Compound _ s _) = extractFieldsForNewScope s
extractFieldsForNewScope (G.Record _ s) = Just s
extractFieldsForNewScope _ = Nothing

extractFunParamsForNewScope :: G.Type -> Maybe [ArgDeclaration]
extractFunParamsForNewScope (G.Callable _ s) = Just s
extractFunParamsForNewScope _ = Nothing

parseErrors :: [L.Token] -> ST.ParserMonad a
parseErrors errors =
  let L.Token {L.aToken=abst, L.posn=pn} = errors !! 0
      tk = errors !! 0
      name = show tk
      line = show $ L.row pn
      column = show $ L.col pn
      header = "\x1b[1m\x1b[31mYOU DIED!!\x1b[0m Parser error: "
      endmsg = "\n\nFix your syntax errors, ashen one."
      position = "line \x1b[1m\x1b[31m" ++ line ++ "\x1b[0m, column \x1b[1m\x1b[31m" ++ column ++ "\x1b[0m."
      msg = header ++ "Unexpected token \x1b[1m\x1b[31m" ++ name ++ "\x1b[0m at " ++ position ++ endmsg
  in  fail msg

addIdsToSymTable :: [NameDeclaration] -> ST.ParserMonad ()
addIdsToSymTable ids = do
  RWS.mapM_ (addIdToSymTable Nothing) ids

addIdToSymTable :: Maybe Int -> NameDeclaration -> ST.ParserMonad ()
addIdToSymTable mi d@(c, (G.Id tk@(L.Token {L.aToken=at, L.cleanedString=idName})), t) = do
  maybeIdEntry <- ST.dictLookup idName
  maybeTypeEntry <- findTypeOnEntryTable t
  (_, (currScope:_), _) <- RWS.get
  case maybeIdEntry of
    -- The name doesn't exists on the table, we just add it
    Nothing -> insertIdToEntry mi t
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
      then insertIdToEntry mi t ST.DictionaryEntry
        { ST.name = idName
        , ST.category = c
        , ST.scope = currScope
        , ST.entryType = ST.name <$> maybeTypeEntry
        , ST.extra = []
        }
      else RWS.tell $ [ST.SemanticError ("Name " ++ idName ++ " was already declared on this scope") tk]


insertIdToEntry :: Maybe Int -> G.Type -> ST.DictionaryEntry -> ST.ParserMonad ()
insertIdToEntry mi t entry = do
  ex <- buildExtraForType t
  let pos = (case mi of
              Nothing -> []
              Just i -> [ST.ArgPosition i])
  ST.addEntry entry{ST.extra = pos ++ ex}
  -- To add the record params to the dictionary
  case extractFieldsForNewScope t of
    Nothing -> return ()
    Just s ->  do
      ST.enterScope
      addIdsToSymTable $ map (\(a, b) -> (ST.RecordItem, a, b)) s
      ST.exitScope
  case extractFunParamsForNewScope t of
    -- If it is nothing then this is not a function
    Nothing -> return ()
    Just s -> do
      ST.enterScope
      RWS.mapM_ (\(i, n) -> addIdToSymTable (Just i) n) $ zip [0..] $
        map (\(argType, i, t) -> (if argType == G.Val
                             then ST.ValueParam
                             else ST.RefParam, i, t)) s

checkConstantReassignment :: G.Expr -> ST.ParserMonad ()
checkConstantReassignment (G.IdExpr (G.Id tk@(L.Token {L.cleanedString=idName}))) = do
  maybeEntry <- ST.dictLookup idName
  case maybeEntry of
    Nothing -> do
      return ()
    Just e ->
      case (ST.category e) of
        ST.Constant -> do
          RWS.tell [ST.SemanticError ("Name " ++ idName ++ " is a constant and must not be reassigned") tk]
          return ()
        _ ->
          return ()
checkConstantReassignment (G.IndexAccess gId _) = checkConstantReassignment gId
checkConstantReassignment _ = return ()

checkIdAvailability :: G.Id -> ST.ParserMonad (Maybe ST.DictionaryEntry)
checkIdAvailability (G.Id tk@(L.Token {L.cleanedString=idName})) = do
  maybeEntry <- ST.dictLookup idName
  case maybeEntry of
    Nothing -> do
      RWS.tell [ST.SemanticError ("Name " ++ idName ++ " is not available on this scope") tk]
      return Nothing
    Just e -> do
      return $ Just e

-- The following function only have sense (for the moment) on lvalues
--  - Ids
--  - Records
--  - Arrays
checkPropertyAvailability :: G.Expr -> ST.ParserMonad ()

-- If it is a record accessing, we need to find the _scope_ of the
-- left side of the expression where to search for the variable
checkPropertyAvailability a@(G.Access expr gId@(G.Id tk@(L.Token {L.cleanedString=s}))) = do
  maybeScope <- findScopeToSearchOf expr
  case maybeScope of
    Nothing -> RWS.tell [ST.SemanticError ("Property " ++ s ++ " does not exists") tk]
    Just s -> do
      (dict, scopes, curr) <- RWS.get
      RWS.put (dict, s:scopes, curr)
      checkIdAvailability gId
      RWS.put (dict, scopes, curr)

checkPropertyAvailability _ = error "invalid usage of checkPropertyAvailability"

extractFieldsFromExtra :: [ST.Extra] -> ST.Extra
extractFieldsFromExtra [] = error "The `extra` array doesn't have any `Fields` item"
extractFieldsFromExtra (s@ST.Fields{} : _) = s
extractFieldsFromExtra (_:ss) = extractFieldsFromExtra ss

findScopeToSearchOf :: G.Expr -> ST.ParserMonad (Maybe ST.Scope)
-- The scope of an id is just the scope of its entry
findScopeToSearchOf (G.IdExpr gId) = do
  maybeEntry <- checkIdAvailability gId
  case maybeEntry of
    Nothing -> return Nothing
    Just ST.DictionaryEntry {ST.extra=extra} -> do
      let (ST.Fields s) = extractFieldsFromExtra extra
      return $ Just s

-- The scope of an record accessing is the scope of its accessing property
findScopeToSearchOf (G.Access expr gId) = do
  maybeScopeOf <- findScopeToSearchOf expr
  case maybeScopeOf of
    Nothing -> return Nothing
    Just s -> do
      (dict, scopes, curr) <- RWS.get
      RWS.put (dict, s:scopes, curr)
      scope <- findScopeToSearchOf $ G.IdExpr gId
      RWS.put (dict, scopes, curr)
      return scope

-- The scope of a index acces is the scope of it's id
findScopeToSearchOf (G.IndexAccess expr _) = findScopeToSearchOf expr

addFunction :: NameDeclaration -> ST.ParserMonad (Maybe (ST.Scope, G.Id))
addFunction d@(_, i@(G.Id tk@(L.Token {L.cleanedString=idName})), _) = do
  (dict, stack, currScope) <- RWS.get
  maybeEntry <- ST.dictLookup idName
  case maybeEntry of
    Nothing -> do
      addIdToSymTable Nothing d
      return $ Just (currScope, i)
    Just entry -> do
      if ST.scope entry == currScope
      then do
        RWS.tell [ST.SemanticError ("Name " ++ idName ++ " was already declared on this scope") tk]
        return Nothing
      else do
        addIdToSymTable Nothing d
        return $ Just (currScope, i)

updateCodeBlockOfFun :: ST.Scope -> G.Id -> G.CodeBlock -> ST.ParserMonad ()
updateCodeBlockOfFun currScope (G.Id tk@(L.Token {L.cleanedString=idName})) code = do
  let f x = (if and [ST.scope x == currScope, ST.name x == idName, ST.category x `elem` [ST.Function, ST.Procedure]]
            then let e = ST.extra x in x{ST.extra = (ST.CodeBlock code) : e}
            else x)
  ST.updateEntry (\ds -> Just $ map f ds) idName

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

-- For functions
findTypeOnEntryTable (G.Callable (Just t) _) = findTypeOnEntryTable t

findTypeOnEntryTable (G.Callable Nothing _) = return Nothing

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

buildExtraForType t@(G.Compound _ tt@(G.Compound{}) maybeExpr) = do
  extra' <- head <$> buildExtraForType tt
  constructor <- (ST.name . fromJust) <$> findTypeOnEntryTable t -- This call should never fail
  return $ case maybeExpr of
    Just e -> [ST.CompoundRec constructor e extra']
    Nothing -> [ST.Recursive constructor extra']

buildExtraForType t@(G.Compound _ tt@(G.Record{}) maybeExpr) = do
  extra' <- head <$> buildExtraForType tt
  constructor <- (ST.name . fromJust) <$> findTypeOnEntryTable t -- This call should never fail
  return $ case maybeExpr of
    Just e -> [ST.CompoundRec constructor e extra', extra']
    Nothing -> [ST.Recursive constructor extra', extra']

buildExtraForType t@(G.Record _ _) = do
  (_, _, currScope) <- RWS.get
  return [ST.Fields $ currScope + 1]

buildExtraForType t@(G.Callable _ []) = return [ST.EmptyFunction]

buildExtraForType t@(G.Callable _ _) = do
  (_, _, currScope) <- RWS.get
  return [ST.Fields $ currScope + 1]


-- For anything else
buildExtraForType _ = return []

class TypeCheckable a where
  getType :: a -> ST.ParserMonad String
  typeMatches :: a -> a -> ST.ParserMonad Bool
  typeMatches a b = do
    aType <- getType a
    bType <- getType b
    return (aType == bType)

isOneOfTypes :: [String] -> G.Expr -> ST.ParserMonad Bool
isOneOfTypes ts a = do
  t <- getType a
  let isValidType = or [t == x | x <- ts]
  return (
    if isValidType then
      True
    else
      False
    )

isLogicalType :: G.Expr -> ST.ParserMonad Bool
isLogicalType = isOneOfTypes [ST.bonfire]

isNumberType :: G.Expr -> ST.ParserMonad Bool
isNumberType = isOneOfTypes [ST.humanity, ST.smallHumanity, ST.hollow]

isComparableType :: G.Expr -> ST.ParserMonad Bool
isComparableType = isOneOfTypes [ST.humanity, ST.smallHumanity, ST.hollow]

isEquatableType :: G.Expr -> ST.ParserMonad Bool
isEquatableType = isOneOfTypes [ST.humanity, ST.smallHumanity, ST.hollow, ST.sign, ST.bonfire]

isShowableType :: G.Expr -> ST.ParserMonad Bool
isShowableType = isOneOfTypes [ST.sign, ST.miracle] -- TODO: check if this is okay

conditionalCheck :: (G.Expr -> ST.ParserMonad Bool) -> G.Expr -> G.Expr ->  ST.ParserMonad String
conditionalCheck condition a b = do
  matches <- typeMatches a b
  aType <- getType a
  isValidType <- condition a
  return (
    if matches then
      if (isValidType) then
        aType
      else
        ST.errorType
    else
      ST.errorType
    )

conditionalCheckReturningBonfire ::  (G.Expr -> ST.ParserMonad Bool) -> G.Expr -> G.Expr ->  ST.ParserMonad String
conditionalCheckReturningBonfire condition a b = do
  matchingType <- conditionalCheck condition a b
  return (
    if matchingType == ST.errorType then
      ST.errorType
    else
      ST.bonfire
    )

arithmeticCheck :: G.Expr -> G.Expr -> ST.ParserMonad String
arithmeticCheck = conditionalCheck isNumberType

comparableCheck :: G.Expr -> G.Expr -> ST.ParserMonad String
comparableCheck = conditionalCheckReturningBonfire isComparableType

equatableCheck :: G.Expr -> G.Expr -> ST.ParserMonad String
equatableCheck = conditionalCheckReturningBonfire isEquatableType

logicalCheck :: G.Expr -> G.Expr -> ST.ParserMonad String
logicalCheck = conditionalCheckReturningBonfire isLogicalType

instance TypeCheckable G.Expr where
  getType G.TrueLit = return (ST.bonfire)
  getType G.FalseLit = return (ST.bonfire)
  getType G.UndiscoveredLit = return (ST.bonfire)
  getType G.NullLit = return (ST.void)
  getType (G.IntLit _) = return (ST.humanity)
  getType (G.FloatLit _) = return (ST.hollow)
  getType (G.CharLit _) = return (ST.sign)
  getType (G.StringLit _) = return (ST.miracle)
  getType (G.ArrayLit _) = return (ST.chest) -- TODO: Recursive type
  getType (G.SetLit _) = return (ST.armor) -- TODO: Recursive type
  getType (G.EvalFunc id _) = getType (G.IdExpr id) -- TODO: Check if okay
  getType (G.Add a b) = arithmeticCheck a b
  getType (G.Substract a b) = arithmeticCheck a b
  getType (G.Multiply a b) = arithmeticCheck a b
  getType (G.Divide a b) = arithmeticCheck a b
  getType (G.Mod a b) = arithmeticCheck a b
  getType (G.Negative a) = arithmeticCheck a a -- cheating
  getType (G.Lt a b) = comparableCheck a b
  getType (G.Lte a b) = comparableCheck a b
  getType (G.Gt a b) = comparableCheck a b
  getType (G.Gte a b) = comparableCheck a b
  getType (G.Eq a b) = equatableCheck a b
  getType (G.Neq a b) = equatableCheck a b
  getType (G.And a b) = logicalCheck a b
  getType (G.Or a b) = logicalCheck a b
  getType (G.Not a) = logicalCheck a a -- cheating
  getType (G.Access e i) = return ("missing") -- TODO: Accessor type
  getType (G.IndexAccess e1 e2) = return ("missing") -- TODO: Accessor type
  getType (G.MemAccess e) = return ("missing") -- TODO: Mem access
  getType (G.IdExpr (G.Id tk@(L.Token _ mid _))) = do
    case mid of
      Nothing -> do
        RWS.tell [ST.SemanticError ("Id " ++ (show tk) ++ " inappropriately initialized") tk]
        return (ST.errorType)
      Just id -> do
        mde <- ST.dictLookup id
        case mde of
          Nothing -> do
            RWS.tell [ST.SemanticError ("Id " ++ id ++ " not found in symbol table") tk]
            return (ST.errorType)
          Just de -> do
            case ST.entryType de of
              Nothing -> do
                RWS.tell [ST.SemanticError ("Id " ++ id ++ " has no type in symbol table") tk]
                return (ST.errorType)
              Just t ->
                return (t)
  getType _ = return ("missing") -- TODO: Finish implementation

checkTypes :: G.Expr -> G.Expr -> String -> L.Token -> ST.ParserMonad ()
checkTypes a b t tk = do
  let isError = t == ST.errorType
  case isError of
    True -> do
      RWS.tell [ST.SemanticError ("Type mismatch between " ++ (show a) ++ " and " ++ (show b)) tk]
      return ()
    False ->
      return ()

checkType :: G.Expr -> String -> L.Token -> ST.ParserMonad ()
checkType a t tk = do
  let isError = t == ST.errorType
  case isError of
    True -> do
      RWS.tell [ST.SemanticError ("Type mismatch for " ++ (show a)) tk]
      return ()
    False ->
      return ()
}
