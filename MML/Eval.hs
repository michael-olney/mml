module MML.Eval (eval, macroError) where

import qualified Data.Map as M
import Control.Monad
import Control.Exception (catch, SomeException, displayException)
import System.Exit

import MML.Types

tbToStr :: String -> Traceback -> String
tbToStr msg ((TracebackRecord name line col macroname):xs) =
    "Macro error: "
    ++ name ++ ":"
    ++ (show line) ++ ":"
    ++ (show col) ++ ":"
    ++ macroname ++ ":\n"
    ++ (tbToStr msg xs)
tbToStr msg [] = msg ++ "\n"

macroError :: Ctx -> String -> IO a
macroError (Ctx tb _ _) msg = do
    putStr . (tbToStr msg) $ tb
    putStr "\n"
    exitWith . ExitFailure $ 1

runMacro :: (Ctx -> [Exp] -> IO [Exp]) -> Ctx -> String -> [Exp] -> IO [Exp]
runMacro evalFun ctx@(Ctx _ params funs) name c
        | M.member name funs = do
            let call = (funs M.! name) ctx evalFun c
            let hdlr m = (macroError ctx) $ displayException (m::SomeException)
            catch call hdlr
        | otherwise = error ("no such macro '" ++ name ++ "'")

runExps ctx xs =
    do
        ys <- mapM (runExp ctx) xs
        return . concat $ ys

runOptExps ctx Nothing      = return Nothing
runOptExps ctx (Just cs)    = (runExps ctx cs) >>= return . Just

runTagExp :: Ctx -> String -> [(Exp, [Exp])] -> Maybe [Exp] -> IO [Exp]
runTagExp ctx@(Ctx tb params funs) name as cs
    | M.member name params                          = return (params M.! name)
    | otherwise                                     = do
        cs' <- runOptExps ctx cs
        let (ks, vs) = unzip as
        vs'<- mapM (runExps ctx) vs
        return [Tag (Str name) (zip ks vs') cs']

runExp :: Ctx -> Exp -> IO [Exp]
runExp ctx@(Ctx ctxtb params funs) (Call calltb nameexp cs)  = do
    let newCtx = (Ctx (calltb:ctxtb) params funs)
    name <- (case nameexp of
            (Str name)  -> return name
            _           -> macroError ctx "macro name must be STRING"
            )
    runMacro runExps newCtx name cs
runExp ctx (Tag (Str name) as cs)   = runTagExp ctx name as cs
runExp ctx (Tag _ as cs)            =
    macroError ctx "tag name must be STRING by the time the tag is evaluated"
runExp ctx c@(Str _)                                = return [c]

eval :: Ctx -> Doc -> IO Doc
eval ctx cs = runExps ctx cs
