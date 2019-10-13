module ProceduresSpec where

import Test.Hspec
import Grammar
import Utils

spec :: Spec
spec = do
    describe "Procedures declarations" $ do
        let buildProgram c = "\
        \ hello ashen one \
        \   " ++ c ++ " \
        \ traveling somewhere \
        \   with orange saponite say @@ \
        \ you died \
        \ farewell ashen one"

        it "accepts no-args procedures" $
            runTestForValidProgram (buildProgram "\
            \ spell fun \
            \   traveling somewhere \
            \   with orange saponite say @@ \
            \   you died \
            \ ashen estus flask consumed") (\(Program _ [
                Procedure (Id "fun") [] _
            ] _) -> True)
        it "accepts 1 val-arg procedure" $
            runTestForValidProgram (buildProgram "\
            \ spell fun \
            \ requesting \
            \   val a of type sign \
            \   traveling somewhere \
            \   with orange saponite say @@ \
            \   you died \
            \ ashen estus flask consumed") (\(Program _ [
                Procedure (Id "fun") [
                    MethodDeclaration Val (Id "a") CharT
                ] _
            ] _) -> True)
        it "accepts 1 ref-arg procedures" $
            runTestForValidProgram (buildProgram "\
            \ spell fun \
            \ requesting \
            \   ref a of type sign \
            \   traveling somewhere \
            \   with orange saponite say @@ \
            \   you died \
            \ ashen estus flask consumed") (\(Program _ [
                Procedure (Id "fun") [
                    MethodDeclaration Ref (Id "a") CharT
                ] _
            ] _) -> True)
        it "accepts several args procedures" $
            runTestForValidProgram (buildProgram "\
            \ spell fun \
            \ requesting \
            \   val a of type sign, \
            \   ref b of type bonfire \
            \   traveling somewhere \
            \   with orange saponite say @@ \
            \   you died \
            \ ashen estus flask consumed") (\(Program _ [
                Procedure (Id "fun") [
                    MethodDeclaration Ref (Id "b") BoolT,
                    MethodDeclaration Val (Id "a") CharT
                ] _
            ] _) -> True)
