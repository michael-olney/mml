module MML.Eval (eval) where

import qualified Data.Map as M
import Control.Monad

import MML.Types

runMacro :: ([Cont] -> IO [Cont]) -> MacroFuns -> String -> [Cont] -> IO [Cont]
runMacro evalFun funs name c
                        | M.member name funs = (funs M.! name) evalFun c
                        | otherwise = error ("no such macro '" ++ name ++ "'")

runConts params funs xs =
    do
        ys <- mapM (runCont params funs) (collapse xs)
        return . collapse . concat $ ys

runOptConts params funs Nothing     = return Nothing
runOptConts params funs (Just cs)   =
    (runConts params funs cs) >>= return . Just

runCont :: Params -> MacroFuns -> Cont -> IO [Cont]
runCont params funs (Macro name cs) =
    runMacro (runConts params funs) funs name cs
runCont params funs (Tag name as cs)
    | M.member name params = return (params M.! name)
    | otherwise = do
        cs' <- runOptConts params funs cs
        let (ks, vs) = unzip as
        vs'<- mapM (runConts params funs) vs
        return [Tag name (zip ks vs') cs']
runCont params funs c@(Str _) = return [c]

collapse :: [Cont] -> [Cont]
collapse xs =
    let
        aux e@(Str _) = [e]
        aux (Tag name attrs children) =
            let
                children2 = (liftM auxList) . (liftM join2) $ children
                (keys, vals) = unzip attrs
                vals2 = map (auxList . join2) vals
                attrs2 = zip keys vals2
            in
                [Tag name attrs2 children2]
        aux (Macro name children) =
            let
                children2 = auxList . join2 $ children
            in
                [Macro name children2]
        auxList = concatMap aux
        join2 (x:y:xs)      | isStr x && isStr y
                            = join2 ((Str ((unwrap1Str x) ++ (unwrap1Str y))):xs)
                            | otherwise
                            = x:(join2 (y:xs))
        join2 [x]           = [x]
        join2 []            = []
    in
        auxList xs

eval :: Params -> MacroFuns -> Doc -> IO Doc
eval params funs (Doc cs) = (runConts params funs cs) >>= return . Doc . collapse
