{
module FireLink.FrontEnd.Preparser (
  preparse) where

import qualified Control.Monad.RWS              as RWS
import           Data.List                      (sort)
import           Data.Maybe                     (catMaybes, isJust, fromJust)
import           FireLink.FrontEnd.Errors
import qualified FireLink.FrontEnd.Grammar      as G
import qualified FireLink.FrontEnd.SymTable     as ST
import qualified FireLink.FrontEnd.Tokens       as T
import qualified FireLink.FrontEnd.TypeChecking as T
import           FireLink.Utils
}

%name                                                                     preparse
%tokentype                                                              { T.Token }
%error                                                                  { parseErrors }
%monad                                                                  { ST.ParserMonad }


%nonassoc size memAccessor

%left ARRCLOSE

%left and or
%nonassoc lt lte gt gte
%left eq neq
%left plus minus
%left mult div mod


%left colConcat
%left diff
%left union intersect

%left isActive
%left asciiOf
%right not
%left accessor
%left arrOpen arrClose


%token
  programBegin                                                          { T.Token {T.aToken=T.TkProgramBegin} }
  programEnd                                                            { T.Token {T.aToken=T.TkProgramEnd} }

  aliasListBegin                                                        { T.Token {T.aToken=T.TkAliasListBegin} }
  aliasListEnd                                                          { T.Token {T.aToken=T.TkAliasListEnd} }
  alias                                                                 { T.Token {T.aToken=T.TkAlias} }

  id                                                                    { T.Token {T.aToken=T.TkId} }

  ofType                                                                { T.Token {T.aToken=T.TkOfType} }

  paramRequest                                                          { T.Token {T.aToken=T.TkRequesting} }
  parVal                                                                { T.Token {T.aToken=T.TkVal} }
  parRef                                                                { T.Token {T.aToken=T.TkRef} }

  bigInt                                                                { T.Token {T.aToken=T.TkBigInt} }
  smallInt                                                              { T.Token {T.aToken=T.TkSmallInt} }
  float                                                                 { T.Token {T.aToken=T.TkFloat} }
  char                                                                  { T.Token {T.aToken=T.TkChar} }
  bool                                                                  { T.Token {T.aToken=T.TkBool} }
  ltelit                                                                { T.Token {T.aToken=T.TkLteLit} }
  string                                                                { T.Token {T.aToken=T.TkString} }
  array                                                                 { T.Token {T.aToken=T.TkArray} }
  set                                                                   { T.Token {T.aToken=T.TkSet} }
  unionStruct                                                           { T.Token {T.aToken=T.TkUnionStruct} }
  record                                                                { T.Token {T.aToken=T.TkRecord} }
  pointer                                                               { T.Token {T.aToken=T.TkPointer} }

  intLit                                                                { T.Token {T.aToken=T.TkIntLit} }
  floatLit                                                              { T.Token {T.aToken=T.TkFloatLit} }
  charLit                                                               { T.Token {T.aToken=T.TkCharLit} }
  stringLit                                                             { T.Token {T.aToken=T.TkStringLit} }
  trueLit                                                               { T.Token {T.aToken=T.TkLit} }
  falseLit                                                              { T.Token {T.aToken=T.TkUnlit} }
  unknownLit                                                            { T.Token {T.aToken=T.TkUndiscovered} }
  nullLit                                                               { T.Token {T.aToken=T.TkNull} }

  functionBegin                                                         { T.Token {T.aToken=T.TkInvocation} }
  functionType                                                          { T.Token {T.aToken=T.TkInvocationType} }
  functionEnd                                                           { T.Token {T.aToken=T.TkInvocationEnd} }

  procedureBegin                                                        { T.Token {T.aToken=T.TkSpell} }
  procedureEnd                                                          { T.Token {T.aToken=T.TkSpellEnd} }

  comma                                                                 { T.Token {T.aToken=T.TkComma} }
  brOpen                                                                { T.Token {T.aToken=T.TkBraceOpen} }
  brClose                                                               { T.Token {T.aToken=T.TkBraceClose} }

  with                                                                  { T.Token {T.aToken=T.TkWith} }
  declarend                                                             { T.Token {T.aToken=T.TkDeclarationEnd} }

  const                                                                 { T.Token {T.aToken=T.TkConst} }
  var                                                                   { T.Token {T.aToken=T.TkVar} }
  asig                                                                  { T.Token {T.aToken=T.TkAsig} }

  instructionsBegin                                                     { T.Token {T.aToken=T.TkInstructionBegin} }
  instructionsEnd                                                       { T.Token {T.aToken=T.TkInstructionEnd} }
  seq                                                                   { T.Token {T.aToken=T.TkSeq} }

  cast                                                                  { T.Token {T.aToken=T.TkCast} }
  offering                                                              { T.Token {T.aToken=T.TkOffering} }
  toTheKnight                                                           { T.Token {T.aToken=T.TkInvocationParsEnd} }

  summon                                                                { T.Token {T.aToken=T.TkSummon} }
  granting                                                              { T.Token {T.aToken=T.TkGranting} }
  toTheEstusFlask                                                       { T.Token {T.aToken=T.TkSpellParsEnd} }

  return                                                                { T.Token {T.aToken=T.TkReturn} }
  returnWith                                                            { T.Token {T.aToken=T.TkReturnWith} }

  print                                                                 { T.Token {T.aToken=T.TkPrint} }
  read                                                                  { T.Token {T.aToken=T.TkRead} }

  whileBegin                                                            { T.Token {T.aToken=T.TkWhile} }
  whileEnd                                                              { T.Token {T.aToken=T.TkEndWhile} }
  covenantIsActive                                                      { T.Token {T.aToken=T.TkCovenantIsActive} }

  ifBegin                                                               { T.Token {T.aToken=T.TkIf} }
  ifEnd                                                                 { T.Token {T.aToken=T.TkEndIf} }
  colon                                                                 { T.Token {T.aToken=T.TkColon} }
  else                                                                  { T.Token {T.aToken=T.TkElse} }

  switchBegin                                                           { T.Token {T.aToken=T.TkSwitch} }
  switchDefault                                                         { T.Token {T.aToken=T.TkSwitchDefault} }
  switchEnd                                                             { T.Token {T.aToken=T.TkEndSwitch} }

  forBegin                                                              { T.Token {T.aToken=T.TkFor} }
  forEnd                                                                { T.Token {T.aToken=T.TkEndFor} }
  souls                                                                 { T.Token {T.aToken=T.TkSoul} }
  untilLevel                                                            { T.Token {T.aToken=T.TkLevel} }

  forEachBegin                                                          { T.Token {T.aToken=T.TkForEach} }
  forEachEnd                                                            { T.Token {T.aToken=T.TkEndForEach} }
  withTitaniteFrom                                                      { T.Token {T.aToken=T.TkWithTitaniteFrom} }

  parensOpen                                                            { T.Token {T.aToken=T.TkParensOpen} }
  parensClose                                                           { T.Token {T.aToken=T.TkParensClose} }

  plus                                                                  { T.Token {T.aToken=T.TkPlus} }
  minus                                                                 { T.Token {T.aToken=T.TkMinus} }
  mult                                                                  { T.Token {T.aToken=T.TkMult} }
  div                                                                   { T.Token {T.aToken=T.TkDiv} }
  mod                                                                   { T.Token {T.aToken=T.TkMod} }
  lt                                                                    { T.Token {T.aToken=T.TkLt} }
  gt                                                                    { T.Token {T.aToken=T.TkGt} }
  lte                                                                   { T.Token {T.aToken=T.TkLte} }
  gte                                                                   { T.Token {T.aToken=T.TkGte} }
  eq                                                                    { T.Token {T.aToken=T.TkEq} }
  neq                                                                   { T.Token {T.aToken=T.TkNeq} }
  not                                                                   { T.Token {T.aToken=T.TkNot} }
  and                                                                   { T.Token {T.aToken=T.TkAnd} }
  or                                                                    { T.Token {T.aToken=T.TkOr} }
  asciiOf                                                               { T.Token {T.aToken=T.TkAsciiOf} }
  isActive                                                              { T.Token {T.aToken=T.TkIsActive} }
  colConcat                                                             { T.Token {T.aToken=T.TkConcat} }
  union                                                                 { T.Token {T.aToken=T.TkUnion} }
  intersect                                                             { T.Token {T.aToken=T.TkIntersect} }
  diff                                                                  { T.Token {T.aToken=T.TkDiff} }
  size                                                                  { T.Token {T.aToken=T.TkSize} }

  arrOpen                                                               { T.Token {T.aToken=T.TkArrayOpen} }
  arrClose                                                              { T.Token {T.aToken=T.TkArrayClose} }
  setOpen                                                               { T.Token {T.aToken=T.TkSetOpen} }
  setClose                                                              { T.Token {T.aToken=T.TkSetClose} }

  accessor                                                              { T.Token {T.aToken=T.TkAccessor} }
  memAccessor                                                           { T.Token {T.aToken=T.TkAccessMemory} }

  malloc                                                                { T.Token {T.aToken=T.TkRequestMemory} }
  free                                                                  { T.Token {T.aToken=T.TkFreeMemory} }
%%

PROGRAM :: { G.Program }
  : programBegin ALIASES METHODS NON_OPENER_CODEBLOCK PROGRAMEND        { G.Program $4 }

PROGRAMEND :: { Maybe G.RecoverableError }
  : programEnd                                                          { Nothing }
  | error                                                               { Just G.MissingProgramEnd }

NON_OPENER_CODEBLOCK :: { G.CodeBlock }
  : instructionsBegin DECLARS INSTRL NON_OPENER_INSTEND                 { G.CodeBlock ($2 ++ reverse $3) 0 }
  | instructionsBegin INSTRL NON_OPENER_INSTEND                         { G.CodeBlock (reverse $2) 0 }

NON_OPENER_INSTEND :: { Maybe G.RecoverableError }
  : instructionsEnd                                                     { Nothing }
  | error                                                               { Just G.MissingInstructionListEnd }

ALIASES :: { () }
  : ALIASLISTBEGIN ALIASL ALIASLISTEND                                  {% do
                                                                            st <- RWS.get
                                                                            ST.popOffset
                                                                            RWS.put st{ST.stScopeStack=[1, 0]} }
  | {- empty -}                                                         { () }

ALIASLISTBEGIN :: { T.Token }
ALIASLISTBEGIN : aliasListBegin                                         {% ST.pushOffset 0 >> return $1 }

ALIASLISTEND :: { Maybe G.RecoverableError }
 : aliasListEnd                                                         { Nothing }
 | error                                                                { Just G.MissingAliasListEnd }

ALIASL :: { () }
  : ALIASL comma ALIASADD                                               { () }
  | ALIASADD                                                            { () }

ALIASADD :: { () }
  : ALIAS                                                               {% do
                                                                            addIdToSymTable $1
                                                                            return () }

ALIAS :: { NameDeclaration }
  : alias ID TYPE                                                       {% do
                                                                            let (ST.Simple i) = $3
                                                                            maybeEntry <- ST.dictLookup i
                                                                            let t = $3
                                                                            let extras = (case maybeEntry of
                                                                                            Nothing -> [t]
                                                                                            Just entry -> [t, ST.findWidth entry])
                                                                            return (ST.Type, $2, extras) }

EXPR :: { G.Expr }
  : intLit                                                              {% preBuildExpr $1 $ G.IntLit (read (T.cleanedString $1) :: Int) }
  | floatLit                                                            {% preBuildExpr $1 $ G.FloatLit (read (T.cleanedString $1) :: Float) }
  | charLit                                                             {% preBuildExpr $1 $ G.CharLit $ head (T.cleanedString $1) }
  | stringLit                                                           {% preBuildExpr $1 $ G.StringLit (T.cleanedString $1) }
  | STRUCTLIT                                                           {% do
                                                                            let (tk, stlit) = $1
                                                                            preBuildExpr tk $ G.StructLit $ reverse stlit }
  | trueLit                                                             {% preBuildExpr $1 G.TrueLit }
  | falseLit                                                            {% preBuildExpr $1 G.FalseLit }
  | unknownLit                                                          {% preBuildExpr $1 G.UndiscoveredLit }
  | nullLit                                                             {% preBuildExpr $1 G.NullLit }
  | arrOpen EXPRL arrClose                                              {% preBuildExpr $1 $ G.ArrayLit $ reverse $2 }
  | setOpen EXPRL setClose                                              {% preBuildExpr $1 $ G.SetLit $ reverse $2 }
  | parensOpen EXPR PARENSCLOSE                                         {  $2{G.expTok=$1} }
  | minus EXPR                                                          {% preBuildExpr $1 $ G.Op1 G.Negate $2 }
  | not EXPR                                                            {% preBuildExpr $1 $ G.Op1 G.Not $2 }
  | asciiOf EXPR                                                        {% preBuildExpr $1 $ G.AsciiOf $2 }
  | isActive EXPR                                                       {% preBuildExpr $1 $ G.IsActive $2 }
  | size EXPR                                                           {% preBuildExpr $1 $ G.Size $2 }
  | EXPR plus EXPR                                                      {% preBuildExpr $2 $ G.Op2 G.Add $1 $3 }
  | EXPR minus EXPR                                                     {% preBuildExpr $2 $ G.Op2 G.Substract $1 $3 }
  | EXPR mult EXPR                                                      {% preBuildExpr $2 $ G.Op2 G.Multiply $1 $3 }
  | EXPR div EXPR                                                       {% preBuildExpr $2 $ G.Op2 G.Divide $1 $3 }
  | EXPR mod EXPR                                                       {% preBuildExpr $2 $ G.Op2 G.Mod $1 $3 }
  | EXPR lt EXPR                                                        {% preBuildExpr $2 $ G.Op2 G.Lt $1 $3 }
  | EXPR gt EXPR                                                        {% preBuildExpr $2 $ G.Op2 G.Gt $1 $3 }
  | EXPR lte EXPR                                                       {% preBuildExpr $2 $ G.Op2 G.Lte $1 $3 }
  | EXPR gte EXPR                                                       {% preBuildExpr $2 $ G.Op2 G.Gte $1 $3 }
  | EXPR eq EXPR                                                        {% preBuildExpr $2 $ G.Op2 G.Eq $1 $3 }
  | EXPR neq EXPR                                                       {% preBuildExpr $2 $ G.Op2 G.Neq $1 $3 }
  | EXPR and EXPR                                                       {% preBuildExpr $2 $ G.Op2 G.And $1 $3 }
  | EXPR or EXPR                                                        {% preBuildExpr $2 $ G.Op2 G.Or $1 $3 }
  | EXPR colConcat EXPR                                                 {% preBuildExpr $2 $ G.Op2 G.ColConcat $1 $3 }
  | EXPR union EXPR                                                     {% preBuildExpr $2 $ G.Op2 G.SetUnion $1 $3 }
  | EXPR intersect EXPR                                                 {% preBuildExpr $2 $ G.Op2 G.SetIntersect $1 $3 }
  | EXPR diff EXPR                                                      {% preBuildExpr $2 $ G.Op2 G.SetDifference $1 $3 }
  | FUNCALL                                                             {% let (tk, i, params) = $1 in preBuildExpr tk $ G.EvalFunc i params }
  | ID                                                                  {% do
                                                                            let (G.Id tk _) = $1
                                                                            preBuildExpr tk $ G.IdExpr $1 }
  | EXPR accessor ID                                                    {% do
                                                                            let expr = G.Access $1 $3
                                                                            preBuildExpr $2 expr }
  | EXPR arrOpen EXPR arrClose                                          {% preBuildExpr $2 $ G.IndexAccess $1 $3 }
  | memAccessor EXPR                                                    {% preBuildExpr $1 $ G.MemAccess $2 }

STRUCTLIT :: { (T.Token, [(G.Id, G.Expr)]) }
  : brOpen PROPLIST BRCLOSE                                             {  ($1, $2) }

PROPLIST :: { [(G.Id, G.Expr)]}
  : PROP                                                                { [$1] }
  | PROPLIST comma PROP                                                 { $3:$1 }

PROP :: { (G.Id, G.Expr) }
  : ID asig EXPR                                                        { ($1, $3) }

EXPRL :: { [G.Expr] }
  : {- empty -}                                                         { [] }
  | NONEMPTYEXPRL                                                       { $1 }

NONEMPTYEXPRL :: { [G.Expr] }
  : EXPR                                                                { [$1] }
  | NONEMPTYEXPRL comma EXPR                                            { $3:$1 }

METHODS :: { () }
  : {- empty -}                                                         {% ST.pushOffset 0 }
  | METHODL                                                             {% ST.pushOffset 0 }

METHODL :: { () }
  : METHODL METHOD                                                      { () }
  | METHOD                                                              { () }

METHOD :: { () }
  : FUNC                                                                {% do
                                                                          st <- RWS.get
                                                                          ST.popOffset
                                                                          RWS.put st{ST.stScopeStack=[1, 0]} }
  | PROC                                                                {% do
                                                                          st <- RWS.get
                                                                          ST.popOffset
                                                                          RWS.put st{ST.stScopeStack=[1, 0]} }

FUNC :: { () }
  : FUNCPREFIX CODEBLOCK functionEnd                                    {% ST.popVisitedMethod }

FUNCPREFIX :: { Maybe (ST.Scope, G.Id) }
  : FUNCNAME METHODPARS functionType TYPE                               {% do
                                                                          currScope <- ST.getCurrentScope
                                                                          ST.exitScope
                                                                          let extras = [ $4, ST.Fields ST.Callable currScope ]
                                                                          ret <- addFunction (ST.Function, $1, extras)
                                                                          ST.pushScope currScope

                                                                          return ret }

FUNCNAME :: { G.Id }
FUNCNAME : functionBegin ID                                             {% ST.enterScope >> ST.pushOffset 0 >> return $2 }

PROC :: { () }
  : PROCPREFIX CODEBLOCK procedureEnd                                   {% ST.popVisitedMethod }

PROCPREFIX :: { Maybe (ST.Scope, G.Id) }
  : PROCNAME PROCPARS                                                   {% do
                                                                            currScope <- ST.getCurrentScope
                                                                            ST.exitScope
                                                                            let extras = [ ST.Simple ST.void, ST.Fields ST.Callable currScope ]
                                                                            st <- RWS.get >>= return . ST.stDict

                                                                            ret <- addFunction (ST.Procedure, $1, extras)
                                                                            ST.pushScope currScope
                                                                            return ret }

PROCNAME :: { G.Id }
PROCNAME : procedureBegin ID                                            {% ST.enterScope >> ST.pushOffset 0 >> return $2 }

PROCPARS :: { Int }
PROCPARS : paramRequest PARS toTheEstusFlask                            {% ST.resetArgPosition >> return $2 }
  | {- empty -}                                                         { 0 }

METHODPARS :: { Int }
  : paramRequest PARS                                                   {% ST.resetArgPosition >> return $2 }
  | {- empty -}                                                         { 0 }

PARS :: { Int }
  : PARS comma PAR                                                      { 1 + $1 }
  | PAR                                                                 { 1 }

PAR :: { () }
  : PARTYPE ID ofType TYPE                                              {% do
                                                                          argPosition <- ST.genNextArgPosition
                                                                          let extras = [$4, ST.ArgPosition argPosition]
                                                                          addIdToSymTable ($1, $2, extras) }

PARTYPE :: { ST.Category }
  : parVal                                                              { ST.ValueParam }
  | parRef                                                              { ST.RefParam }

{- TYPE always returns "ST.Simple String" -}
TYPE :: { ST.Extra }
  : ID                                                                  {% do
                                                                          let G.Id t _ = $1
                                                                          return $ ST.Simple (T.cleanedString t) }
  | bigInt                                                              { ST.Simple ST.humanity }
  | smallInt                                                            { ST.Simple ST.smallHumanity }
  | float                                                               { ST.Simple ST.hollow }
  | char                                                                { ST.Simple ST.sign }
  | bool                                                                { ST.Simple ST.bonfire }
  | ltelit EXPR array ofType TYPE                                       {% createAnonymousAlias $1 $
                                                                              [ ST.CompoundRec (T.cleanedString $3) $2 $5
                                                                              , ST.Width $ ST.wordSize * 2 ] }

  | ltelit EXPR string                                                  {% createAnonymousAlias $1 $
                                                                              [ ST.Compound (T.cleanedString $3) $2
                                                                              , ST.Width $ ST.wordSize * 2 ] }
  | set ofType TYPE                                                     {% createAnonymousAlias $1 $
                                                                              [ ST.Recursive (T.cleanedString $1) $3
                                                                              , ST.Width ST.wordSize ] }
  | pointer TYPE                                                        {% createAnonymousAlias $1 $
                                                                              [ ST.Recursive (T.cleanedString $1) $2
                                                                              , ST.Width ST.wordSize ] }
  | RECORD_OPEN  brOpen STRUCTITS BRCLOSE                               {% do
                                                                            currScope <- ST.getCurrentScope
                                                                            nextOffset <- ST.getNextOffset
                                                                            ST.exitScope
                                                                            ST.popOffset
                                                                            ret <- createAnonymousAlias $1 $ [
                                                                              ST.Fields ST.Record currScope, ST.Width nextOffset]
                                                                            return ret }
  | UNION_OPEN brOpen UNIONITS BRCLOSE                                  {% do
                                                                             currScope <- ST.getCurrentScope
                                                                             ST.exitScope
                                                                             ST.popUnion
                                                                             ST.popOffset
                                                                             ret <- createAnonymousAlias $1 $ [ST.Fields ST.Union currScope,
                                                                              ST.Width $ 4 + $3]
                                                                             return ret }

RECORD_OPEN :: { T.Token }
RECORD_OPEN : record                                                    {% ST.enterScope >> ST.pushOffset 0 >> return $1 }

UNION_OPEN :: { T.Token }
UNION_OPEN : unionStruct                                                {% ST.enterScope >> ST.pushOffset 0 >> ST.newUnion >> return $1 }

BRCLOSE :: { Maybe G.RecoverableError }
  : brClose                                                             { Nothing }
  | error                                                               { Just G.MissingClosingBrace }

PARENSCLOSE :: { Maybe G.RecoverableError }
  : parensClose                                                         { Nothing }
  | error                                                               { Just G.MissingClosingParens }

UNIONITS :: { Int }
  : UNIONITS comma UNIONIT                                             { max $1 $3 }
  | UNIONIT                                                            { $1 }

UNIONIT :: { Int }
  : ID ofType TYPE                                                      {% do
                                                                            unionAttrId <- fmap ST.UnionAttrId ST.genNextUnion
                                                                            addIdToSymTable (ST.UnionItem, $1, [$3, unionAttrId])
                                                                            let ST.Simple t = $3
                                                                            maybeTypeEntry <- ST.dictLookup t
                                                                            case maybeTypeEntry of
                                                                              Nothing -> return 0
                                                                              Just entry -> do
                                                                                let ST.Width w = ST.findWidth entry
                                                                                return w }

STRUCTITS :: { () }
  : STRUCTITS comma STRUCTIT                                            { () }
  | STRUCTIT                                                            { () }

STRUCTIT :: { () }
  : ID ofType TYPE                                                      {% addIdToSymTable (ST.RecordItem, $1, [$3]) }

ID :: { G.Id }
  : id                                                                  {% do
                                                                            currScope <- ST.getCurrentScope
                                                                            return $ G.Id $1 currScope }

CODEBLOCK :: { G.CodeBlock }
  : INSTBEGIN DECLARS INSTRL INSTEND                                    { G.CodeBlock ($2 ++ reverse $3) 0 }
  | INSTBEGIN INSTRL INSTEND                                            { G.CodeBlock (reverse $2) 0 }

INSTBEGIN :: { T.Token }
INSTBEGIN : instructionsBegin                                           {% do
                                                                            ST.enterScope
                                                                            nextOffset <- ST.getNextOffset
                                                                            ST.pushOffset nextOffset
                                                                            st <- RWS.get
                                                                            return $1 }

INSTEND :: { Maybe G.RecoverableError }
  : instructionsEnd                                                     {% do
                                                                             ST.exitScope
                                                                             ST.popOffset
                                                                             return Nothing }
  | error                                                               { Just G.MissingInstructionListEnd }

DECLARS :: { [G.Instruction] }
  : with DECLARSL DECLAREND                                             { catMaybes (reverse $2) }

DECLAREND :: { Maybe G.RecoverableError }
  : declarend                                                           { Nothing }
  | error                                                               { Just G.MissingDeclarationListEnd }

DECLARSL :: { [Maybe G.Instruction] }
  : DECLARSL comma DECLARADD                                            { $3:$1 }
  | DECLARADD                                                           { [$1] }

DECLARADD :: { Maybe G.Instruction }
  : DECLAR                                                              {% do
                                                                            -- Adds a declaration to the ST as soon as it's parsed
                                                                            let (declar, _) = $1
                                                                            let ((_, lvalueId, _), maybeRValue) = $1
                                                                            case maybeRValue of
                                                                              Nothing -> return Nothing
                                                                              Just (token, rvalue) -> do
                                                                                let baseLvalue = G.IdExpr lvalueId
                                                                                let (G.Id idToken _) = lvalueId
                                                                                lvalue <- preBuildExpr idToken baseLvalue
                                                                                return $ Just $ G.InstAsig lvalue rvalue }

DECLAR :: { (NameDeclaration, Maybe (T.Token, G.Expr)) }
  : var ID ofType TYPE                                                  { ((ST.Variable, $2, [$4]), Nothing) }
  | var ID ofType TYPE asig EXPR                                        { ((ST.Variable, $2, [$4]), Just ($5, $6))  }
  | const ID ofType TYPE asig EXPR                                      { ((ST.Constant, $2, [$4]), Just ($5, $6)) }

INSTRL :: { G.Instructions }
  : INSTRL seq INSTR                                                    { $3 : $1 }
  | INSTR                                                               { [$1] }

INSTR :: { G.Instruction }
  : EXPR asig EXPR                                                      { G.InstAsig $1 $3 }
  | malloc EXPR                                                         { G.InstMalloc $2 }
  | free EXPR                                                           { G.InstFreeMem $2 }
  | cast ID PROCARGS                                                    { G.InstCall $2 $3 }
  | FUNCALL                                                             { let (tk, i, params) = $1 in
                                                                          G.InstCall i params }
  | return                                                              { G.InstReturn }
  | returnWith EXPR                                                     { G.InstReturnWith $2 }
  | print EXPR                                                          { G.InstPrint $2 }
  | read EXPR                                                           { G.InstRead $2 }
  | whileBegin EXPR covenantIsActive COLON CODEBLOCK WHILEEND           { G.InstWhile $2 $5 }
  | ifBegin IFCASES ELSECASE IFEND                                      {% do
                                                                          return $ G.InstIf (reverse ($3 : $2)) }
  | ifBegin IFCASES IFEND                                               {% do
                                                                          return $ G.InstIf (reverse $2) }
  | SWITCHBEGIN COLON SWITCHCASES DEFAULTCASE SWITCHEND                 { let (switchTk, switchExpr) = $1 in
                                                                          G.InstSwitch switchExpr (reverse ($4 : $3)) }
  | SWITCHBEGIN COLON SWITCHCASES SWITCHEND                             { let (switchTk, switchExpr) = $1 in
                                                                          G.InstSwitch switchExpr (reverse $3) }
  | FORBEGIN CODEBLOCK FOREND                                           { let (fstTk, iterVar, _, startExpr, stopExpr) = $1 in
                                                                          G.InstFor iterVar startExpr stopExpr $2 }
  | FOREACHBEGIN CODEBLOCK FOREACHEND                                   { let (fstTk, iterVar, _, _, iteredExpr) = $1 in
                                                                          G.InstForEach iterVar iteredExpr $2 }

SWITCHBEGIN :: { (T.Token, G.Expr) }
  : switchBegin EXPR                                                    { ($1, $2) }

FORBEGIN :: { (T.Token, G.Id, Bool, G.Expr, G.Expr) }
  : forBegin ID with EXPR souls untilLevel EXPR                         { ($1, $2, False, $4, $7) }

FOREND :: { Maybe G.RecoverableError }
  : forEnd                                                              { Nothing }
  | error                                                               { Just G.MissingForEnd }

FOREACHBEGIN :: { (T.Token, G.Id, Bool, Bool, G.Expr) }
  : forEachBegin ID withTitaniteFrom EXPR                               { ($1, $2, False, False, $4) }

FOREACHEND :: { Maybe G.RecoverableError }
  : forEachEnd                                                          { Nothing }
  | error                                                               { Just G.MissingForEachEnd }

WHILEEND :: { Maybe G.RecoverableError }
  : whileEnd                                                            { Nothing }
  | error                                                               { Just G.MissingWhileEnd }

COLON :: { Maybe G.RecoverableError }
  : colon                                                               { Nothing }
  | error                                                               { Just G.MissingColon }

FUNCALL :: { (T.Token, G.Id, G.Params) }
  : summon ID FUNCPARS                                                  { ($1, $2, $3) }

IFCASES :: { G.IfCases }
  : IFCASES IFCASE                                                      { $2 : $1 }
  | IFCASE                                                              { [$1] }

IFCASE :: { G.IfCase }
  : EXPR COLON CODEBLOCK                                                { G.GuardedCase $1 $3 }

ELSECASE :: { G.IfCase }
  : else COLON CODEBLOCK                                                { G.GuardedCase (G.Expr
                                                                                          { G.expAst = G.TrueLit
                                                                                          , G.expType = T.TrileanT
                                                                                          , G.expTok = (T.Token
                                                                                                          { T.aToken = T.TkLit
                                                                                                          , T.capturedString = "lit"
                                                                                                          , T.cleanedString = "lit"})
                                                                                          }) $3 }

IFEND :: { Maybe G.RecoverableError }
  : ifEnd                                                               { Nothing }
  | error                                                               { Just G.MissingIfEnd }

SWITCHCASES :: { G.SwitchCases }
  : SWITCHCASES SWITCHCASE                                              { $2 : $1 }
  | SWITCHCASE                                                          { [$1] }

SWITCHCASE :: { G.SwitchCase }
  : EXPR COLON CODEBLOCK                                                { G.Case $1 $3 }

SWITCHEND :: { Maybe G.RecoverableError }
  : switchEnd                                                           { Nothing }
  | error                                                               { Just G.MissingSwitchEnd }

DEFAULTCASE :: { G.SwitchCase }
  : switchDefault COLON CODEBLOCK                                       { G.DefaultCase $3 }

FUNCPARS :: { G.Params }
  : granting PARSLIST TOTHEKNIGHT                                       { reverse $2 }
  | {- empty -}                                                         { [] }

TOTHEKNIGHT :: { Maybe G.RecoverableError }
  : toTheKnight                                                         { Nothing }
  | error                                                               { Just G.MissingFunCallEnd }

PROCARGS :: { G.Params }
  : offering PARSLIST TOTHEESTUSFLASK                                   { reverse $2 }
  | {- empty -}                                                         { [] }

TOTHEESTUSFLASK :: { Maybe G.RecoverableError }
  : toTheEstusFlask                                                     { Nothing }
  | error                                                               { Just G.MissingProcCallEnd }

PARSLIST :: { G.Params }
  : PARSLIST comma EXPR                                                 { $3 : $1 }
  | EXPR                                                                { [$1] }

{

type NameDeclaration = (ST.Category, G.Id, [ST.Extra])
type RecordItem = (G.Id, ST.Extra)

createAnonymousAlias :: T.Token -> [ST.Extra] -> ST.ParserMonad ST.Extra
createAnonymousAlias token extras = do
  anonymousAlias <- ST.genAliasName
  currentScope <- ST.getCurrentScope
  let tk = token{T.cleanedString=anonymousAlias, T.aToken = T.TkId}
  let declaration = (ST.Type, G.Id tk currentScope, extras)
  addIdToSymTable declaration
  return $ ST.Simple anonymousAlias

preBuildExpr :: T.Token -> G.BaseExpr -> ST.ParserMonad G.Expr
preBuildExpr tk bExpr = return G.Expr
  { G.expAst = bExpr,
    -- Proper typechecking will be done at the Parsing stage
    G.expType = T.TypeError,
    G.expTok = tk
  }

parseErrors :: [T.Token] -> ST.ParserMonad a
parseErrors errors =
  let tk@T.Token {T.aToken=abst, T.position=pn} = errors !! 0
      name = show tk
      line = show $ row pn
      column' = show $ column pn
      header = bold ++ red ++ "YOU DIED!!" ++ nocolor ++ " Parser error: "
      endmsg = "\n\nFix your syntax errors, ashen one."
      position = "line " ++ bold ++ red ++ line ++ nocolor ++ ", column " ++ bold ++ red ++ column' ++ nocolor ++ "."
      msg = header ++ "Unexpected token " ++ bold ++ red ++ name ++ nocolor ++ " at " ++ position ++ endmsg
  in  fail msg

addIdToSymTable :: NameDeclaration -> ST.ParserMonad ()
addIdToSymTable d@(c, gId@(G.Id tk@(T.Token {T.aToken=at, T.cleanedString=idName}) _), t) = do
  maybeIdEntry <- ST.dictLookup idName
  let typeEntry = getTypeNameForSymTable $ head $ filter ST.isExtraAType t
  currScope <- ST.getCurrentScope
  case maybeIdEntry of
    -- The name doesn't exists on the table, we just add it
    Nothing ->
      assignOffsetAndInsert ST.DictionaryEntry
        { ST.name = idName
        , ST.category = c
        , ST.scope = currScope
        , ST.entryType = Just typeEntry
        , ST.extra = t
        }

    -- The name does exists on the table, we just add it depending on the scope
    Just entry -> do
      let scope = ST.scope entry
      let category = ST.category entry
      if category == ST.Type && c /= ST.RecordItem
      then logSemError ("Name " ++ show tk ++ " conflicts with an type alias") tk
      else if category == ST.Procedure
      then logSemError ("Name " ++ show tk ++ " conflicts with a procedure") tk
      else if category == ST.Function
      then logSemError ("Name " ++ show tk ++ " conflicts with a function") tk
      else if category == ST.RefParam || category == ST.ValueParam
      then logSemError ("Name " ++ show tk ++ " conflicts with a formal param") tk
      else if currScope /= scope
      then assignOffsetAndInsert ST.DictionaryEntry
        { ST.name = idName
        , ST.category = c
        , ST.scope = currScope
        , ST.entryType = Just typeEntry
        , ST.extra = t
        }
      else logSemError ("Name " ++ idName ++ " was already declared on this scope") tk
  where
    getTypeNameForSymTable :: ST.Extra -> String
    getTypeNameForSymTable (ST.Simple s) = s
    getTypeNameForSymTable (ST.Recursive s _) = s
    getTypeNameForSymTable (ST.Compound s _) = s
    getTypeNameForSymTable (ST.CompoundRec s _ _) = s
    getTypeNameForSymTable (ST.Fields ST.Record _) = ST.bezel
    getTypeNameForSymTable (ST.Fields ST.Union _) = ST.link

assignOffsetAndInsert :: ST.DictionaryEntry -> ST.ParserMonad ()
assignOffsetAndInsert entry = do
  let extras = ST.extra entry
  finalExtras <-
    if requiresOffset then do
      let (ST.Simple t) = ST.extractTypeFromExtra extras
      maybeTypeEntry <- ST.dictLookup t
      case maybeTypeEntry of
        Nothing -> return extras
        Just typeEntry -> do
          nextOffset <- ST.getNextOffset
          let ST.Width realWidth = ST.findWidth typeEntry

          -- Ref params occupies 1 wordSize
          let width = if category == ST.RefParam then ST.wordSize else realWidth

          let o = nextOffset `mod` ST.wordSize
          let remainingOffset = ST.wordSize - o
          let finalOffset = if category == ST.UnionItem then ST.wordSize
                            else if o == 0 || width <= remainingOffset then nextOffset
                            else nextOffset + remainingOffset
          ST.putNextOffset $ finalOffset + width
          return $ (ST.Offset finalOffset) : extras
    else return extras
  ST.addEntry entry{ST.extra=finalExtras}
  where
    category :: ST.Category
    category = ST.category entry
    categoriesThatRequireOffset :: [ST.Category]
    categoriesThatRequireOffset =
      [ ST.ValueParam, ST.RefParam, ST.RecordItem
      , ST.Variable, ST.Constant, ST.UnionItem ]

    requiresOffset :: Bool
    requiresOffset = category `elem` categoriesThatRequireOffset

checkIdAvailability :: G.Id -> ST.ParserMonad (Maybe ST.DictionaryEntry)
checkIdAvailability (G.Id tk@(T.Token {T.cleanedString=idName}) _) = ST.dictLookup idName >>= return

addFunction :: NameDeclaration -> ST.ParserMonad (Maybe (ST.Scope, G.Id))
addFunction d@(_, i@(G.Id tk@(T.Token {T.cleanedString=idName}) _), _) = do
  currScope <- ST.getCurrentScope
  ST.addVisitedMethod idName
  maybeEntry <- ST.dictLookup idName
  case maybeEntry of
    Nothing -> do
      addIdToSymTable d
      return $ Just (currScope, i)
    Just entry -> do
      logSemError ("Function " ++ idName ++ " was already declared") tk
      return Nothing

logSemError :: String -> T.Token -> ST.ParserMonad ()
logSemError msg tk = RWS.tell [Error msg (T.position tk)]

}