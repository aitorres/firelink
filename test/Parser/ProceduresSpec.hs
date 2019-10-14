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
                    MethodDeclaration Val (Id "a") CharT,
                    MethodDeclaration Ref (Id "b") BoolT
                ] _
            ] _) -> True)
        it "accepts several args procedures" $
            runTestForValidProgram (buildProgram "\
            \ spell fun \
            \ requesting \
            \   val a of type sign, \
            \   ref b of type bonfire \
            \   traveling somewhere \
            \   go back \
            \   you died \
            \ ashen estus flask consumed") (\(Program _ [
                Procedure (Id "fun") [
                    MethodDeclaration Val (Id "a") CharT,
                    MethodDeclaration Ref (Id "b") BoolT
                ] (CodeBlock _ [InstReturn])
            ] _) -> True)

    describe "Procedure calls" $ do
        let buildProgram c = "\
        \ hello ashen one \
        \ traveling somewhere \
        \   " ++ c ++ " \
        \ you died \
        \ farewell ashen one"


        it "allows calling no-args procedures" $
            runTestForValidProgram (buildProgram "\
            \ cast f") (\(Program _ _ (CodeBlock _ [
                InstCallProc (Id "f") []
                ])) -> True)

        it "allows calling 1-args procedures" $
            runTestForValidProgram (buildProgram "\
            \ cast f offering lit to the estus flask") (\(Program _ _ (CodeBlock _ [
                InstCallProc (Id "f") [TrueLit]
                ])) -> True)

        it "allows calling 2-args procedures" $
            runTestForValidProgram (buildProgram "\
            \ cast f offering lit, unlit to the estus flask") (\(Program _ _ (CodeBlock _ [
                InstCallProc (Id "f") [TrueLit, FalseLit]
                ])) -> True)

