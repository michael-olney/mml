module MML.Eval (eval) where

import qualified Data.Map as M
import Control.Monad

import MML.Types

runMacro :: ([Exp] -> IO [Exp]) -> MacroFuns -> String -> [Exp] -> IO [Exp]
runMacro evalFun funs name c
                        | M.member name funs = (funs M.! name) evalFun c
                        | otherwise = error ("no such macro '" ++ name ++ "'")

runExps params funs xs =
    do
        ys <- mapM (runExp params funs) (collapse xs)
        return . collapse . concat $ ys

runOptExps params funs Nothing     = return Nothing
runOptExps params funs (Just cs)   =
    (runExps params funs cs) >>= return . Just

runExp :: Params -> MacroFuns -> Exp -> IO [Exp]
runExp params funs (Call name cs) =
    runMacro (runExps params funs) funs name cs
runExp params funs (Tag name as cs)
    | M.member name params = return (params M.! name)
    | otherwise = do
        cs' <- runOptExps params funs cs
        let (ks, vs) = unzip as
        vs'<- mapM (runExps params funs) vs
        return [Tag name (zip ks vs') cs']
runExp params funs c@(Str _) = return [c]

collapse :: [Exp] -> [Exp]
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
        aux (Call name children) =
            let
                children2 = auxList . join2 $ children
            in
                [Call name children2]
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
eval params funs cs
    = (runExps params funs cs) >>= return . collapse
