module FunctionsSpec where

import Test.Hspec
import Grammar
import Utils

spec :: Spec
spec = do
    describe "Functions declarations" $ do
        let buildProgram c = "\
        \ hello ashen one \
        \   " ++ c ++ " \
        \ traveling somewhere \
        \   with orange saponite say @@ \
        \ you died \
        \ farewell ashen one"

        it "accepts 2+ functions" $
            runTestForValidProgram (buildProgram "\
            \ invocation fun \
            \ with skill of type humanity \
            \   traveling somewhere \
            \   with orange saponite say @@ \
            \   you died \
            \ after this return to your world\

            \ invocation hi \
            \ with skill of type sign \
            \   traveling somewhere \
            \   with orange saponite say @@ \
            \   you died \
            \ after this return to your world")  (\(Program _ [
                Function (Id "hi") [] CharT _,
                Function (Id "fun") [] BigInt _
            ] _) -> True)

        it "accepts no-args functions" $
            runTestForValidProgram (buildProgram "\
            \ invocation fun \
            \ with skill of type humanity \
            \   traveling somewhere \
            \   with orange saponite say @@ \
            \   you died \
            \ after this return to your world") (\(Program _ [
                Function (Id "fun") [] BigInt _
            ] _) -> True)
        it "accepts 1 val-arg functions" $
            runTestForValidProgram (buildProgram "\
            \ invocation fun \
            \ requesting \
            \   val a of type sign \
            \ with skill of type humanity \
            \   traveling somewhere \
            \   with orange saponite say @@ \
            \   you died \
            \ after this return to your world") (\(Program _ [
                Function (Id "fun") [
                    MethodDeclaration Val (Id "a") CharT
                ] BigInt _
            ] _) -> True)
        it "accepts 1 ref-arg functions" $
            runTestForValidProgram (buildProgram "\
            \ invocation fun \
            \ requesting \
            \   ref a of type sign \
            \ with skill of type humanity \
            \   traveling somewhere \
            \   with orange saponite say @@ \
            \   you died \
            \ after this return to your world") (\(Program _ [
                Function (Id "fun") [
                    MethodDeclaration Ref (Id "a") CharT
                ] BigInt _
            ] _) -> True)
        it "accepts several args functions" $
            runTestForValidProgram (buildProgram "\
            \ invocation fun \
            \ requesting \
            \   val a of type sign, \
            \   ref b of type bonfire \
            \ with skill of type humanity \
            \   traveling somewhere \
            \   with orange saponite say @@ \
            \   you died \
            \ after this return to your world") (\(Program _ [
                Function (Id "fun") [
                    MethodDeclaration Val (Id "a") CharT,
                    MethodDeclaration Ref (Id "b") BoolT
                ] BigInt _
            ] _) -> True)
        it "accepts functions with return statements" $
            runTestForValidProgram (buildProgram "\
            \ invocation fun \
            \ requesting \
            \   val a of type sign, \
            \   ref b of type bonfire \
            \ with skill of type humanity \
            \   traveling somewhere \
            \   go back with lit \
            \   you died \
            \ after this return to your world") (\(Program _ [
                Function (Id "fun") [
                    MethodDeclaration Val (Id "a") CharT,
                    MethodDeclaration Ref (Id "b") BoolT
                ] BigInt (CodeBlock _ [
                    InstReturnWith TrueLit
                ])
            ] _) -> True)
    describe "Function calls" $ do
        let buildProgram c = "\
        \ hello ashen one \
        \ traveling somewhere \
        \   " ++ c ++ " \
        \ you died \
        \ farewell ashen one"


        it "allows calling no-args functions" $
            runTestForValidProgram (buildProgram "\
            \ summon f") (\(Program _ _ (CodeBlock _ [
                InstCallFunc (Id "f") []
                ])) -> True)

        it "allows calling 1-args functions" $
            runTestForValidProgram (buildProgram "\
            \ summon f granting lit to the knight") (\(Program _ _ (CodeBlock _ [
                InstCallFunc (Id "f") [TrueLit]
                ])) -> True)

        it "allows calling 2-args functions" $
            runTestForValidProgram (buildProgram "\
            \ summon f granting lit, unlit to the knight") (\(Program _ _ (CodeBlock _ [
                InstCallFunc (Id "f") [TrueLit, FalseLit]
                ])) -> True)

