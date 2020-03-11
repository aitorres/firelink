module FireLink.BackEnd.BackEndCompiler (
    backend
) where

import           Control.Monad.RWS                         (runRWST)
import           FireLink.BackEnd.CodeGenerator            (TAC (..), genCode,
                                                            initialState, CodeGenState (..), TACSymEntry (..))
import           FireLink.BackEnd.InstructionCodeGenerator ()
import           FireLink.BackEnd.Optimizer                (optimize)
import           FireLink.FrontEnd.Grammar                 (Program (..))
import           FireLink.FrontEnd.SymTable                (Dictionary (..))
import           FireLink.FrontEnd.TypeChecking            (Type (..))
import           TACType

backend :: Program -> Dictionary -> IO [TAC]
backend program dictionary = do
    (_, finalState, code) <- runRWST (genCode program) dictionary initialState
    print finalState
    return $ optimize $ fillEmptyTemporals (cgsTemporalsToReplace finalState) code
    where
        fillEmptyTemporals :: [(TACSymEntry, Int)] -> [TAC] -> [TAC]
        fillEmptyTemporals tempsToReplace = map (patchTac tempsToReplace)

        patchTac :: [(TACSymEntry, Int)] -> TAC -> TAC
        patchTac tempsToReplace tac = case tac of
            ThreeAddressCode Assign (Just (Id x)) (Just (Constant ("TO_REPLACE", _))) _ ->
                let l = filter (matchTemps x . fst) tempsToReplace in
                    if null l then tac
                    else let t = head l in
                        ThreeAddressCode
                            Assign
                            (Just (Id $ fst t)) 
                            (Just $ Constant (show $ snd t, BigIntT))
                            Nothing
            _ -> tac

        matchTemps :: TACSymEntry -> TACSymEntry -> Bool
        matchTemps (TACTemporal i _) (TACTemporal i' _) = i == i'
        matchTemps _ _ = False
