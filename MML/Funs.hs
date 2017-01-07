{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module MML.Funs (MacroFun, MacroFuns, funs) where

import MML
import MML.Types
import MML.Parse
import MML.Eval
import MML.Scripting (runScript)

import Prelude hiding (readFile)
import Data.Char
import Data.List
import qualified System.IO.Strict as SIO
import System.Exit
import System.IO
import Control.Monad
import Control.Monad.Extra
import Data.ByteString.Lazy as B (writeFile)
import qualified Data.Map as M
import Data.Map ((!))
import Data.List (splitAt)

convParam (Tag (Str name) (M.toList -> []) (Just v)) = (name, v)
convParam (Tag _ (M.toList -> []) _)        = error "shouldn't see this"
convParam _                                 = error "wrong form for parameter"

rawinc :: [Exp] -> IO [Exp]
rawinc ((Str fn):rest) = do
    let bindings = M.fromList . (map convParam) $ rest
    inp <- SIO.readFile fn
    return [Str inp]
raw inc _ = error "wrong form for macro 'rawinc'"

inc :: [Exp] -> IO [Exp]
inc ((Str fn):rest) = do
    let bindings = M.fromList . (map convParam) $ rest
    inp <- SIO.readFile fn
    r <- parse ("include: " ++ fn) inp
    doc <- (case r of
        (Left err)  -> error $ "parse error: " ++ err
        (Right doc) -> return doc
        )
    doc2 <- eval (Ctx [] bindings funs) doc

    return doc2
inc _ = error "wrong form for macro 'inc'"

subst :: Ctx -> [Exp] -> [Exp]
subst ctx@(Ctx tb env funs) = auxList
    where
        aux (Var name)
            | M.member name env     = env ! name
            | otherwise             = error $ "unbound variable: " ++ name
        aux (Tag name attrs children) =
                let
                    attrs2 = M.map auxList attrs
                    [name2] = aux name
                in
                    [Tag name2 attrs2 ((liftM auxList) children)]
        aux e@(Tag _ _ _ )          = [e]
        aux e@(Str _) = [e]
        aux (Call tb name cs) = [Call tb name (auxList cs)]
        auxList = concatMap aux

filesize :: [Exp] -> IO [Exp]
filesize [(Str fn)] = do
    h <- openFile fn ReadMode
    sz <- hFileSize h
    hClose h
    return . (:[]) . Str . show $ sz
filesize e = error ("bad usage for macro 'filesize'" ++ (show e))

kilobyte = 1024 :: Int
megabyte = kilobyte*1024
gigabyte = megabyte*1024
terabyte = gigabyte*1024

units = [
    ("TB", terabyte),
    ("GB", gigabyte),
    ("MB", megabyte),
    ("KB", kilobyte)
    ]

prettyfilesize :: [Exp] -> [Exp]
prettyfilesize [(Str x)] =
    let
        v = read x
        aux [] = (show x) ++ " bytes"
        aux ((units, unitv):xs) = (case v >= unitv of
            True -> (show (v `div` unitv)) ++ units
            False -> aux xs)
    in
        [Str (aux units)]
prettyfilesize _ = error "bad usage of macro prettyfilesize"

nestEnv :: Env -> Env -> Env
nestEnv outer inner = M.union inner outer

caseMacro :: Ctx -> (Ctx -> [Exp] -> IO [Exp]) -> [Exp] -> IO [Exp]
caseMacro ctx@(Ctx tb env macros) evalFun
    (
        (Tag (Str "") (M.toList -> []) (Just xs))
        :ps
        ) =
    let
        newConstr v e constrs
            = if M.member v constrs
                then if constrs ! v == e
                    then return constrs
                    else fail "unification failure"
                else
                    return $ M.insert v e constrs

        match (Tag n0 a0 (Just e0)) (Tag n1 a1 (Just e1))   constrs
            = match n0 n1 constrs
                >>= matchAttrs a0 a1
                >>= matchExps e0 e1
        match (Str x)               (Str y)                 constrs
            | x == y = return constrs
            | x /= y = fail "string mismatch"
        match (Call _ _ _)          _                       constrs
            = error "macro call on LHS in pattern match"
        match _                     (Call _ _ _)            constrs
            = error "macro call on RHS in pattern match"
        match _                     (Var v)                 constrs
            = error $ "variable on RHS in pattern match: " ++ (show v)
        match (Var v)               e                       constrs
            = newConstr v [e] constrs

        matchPair constrs (e0, e1) = match e0 e1 constrs

        matchExps e0 e1 constrs
            | length e0 == length e1
                = foldM matchPair constrs $ zip e0 e1
            | length e0 < length e1 && length e0 > 0
                = do
                    let (pref1, suff1) = splitAt (length e0 - 1) e1
                    let pref0 = take (length e0 - 1) e0
                    let var0 = last e0
                    constrs2 <- foldM matchPair constrs $ zip pref0 pref1
                    (case var0 of
                        (Var v)     -> newConstr v suff1 constrs2
                        _           -> fail "pattern match failed in list"
                        )
            | otherwise
                = fail "pattern match failed in list"

        matchAttrs = fail "attribute pattern matching not yet implemented"

        aux ((Tag (Str "") (M.toList -> []) (Just pat)):
            (Tag (Str "") (M.toList -> []) (Just pbody))
            :ps)= do
                    xs2 <- evalFun ctx xs
                    (case matchExps pat xs2 M.empty of
                        (Just inner)
                            -> evalFun (Ctx tb (nestEnv env inner) macros) pbody
                        Nothing
                            -> aux ps
                        )
        aux []  = fail "pattern match exhausted"
        aux _   = fail "pattern or pattern body missing"
    in
        aux ps
caseMacro _ _ e = error "bad usage of macro case"

def :: Ctx -> (Ctx -> [Exp] -> IO [Exp]) -> [Exp] -> IO [Exp]
def ctx@(Ctx tb env macros) evalFun
    (
        (Str macroname)
        :(Str paramname)
        :(Tag (Str "") (M.toList -> []) (Just bodyexps))
        :(Tag (Str "") (M.toList -> []) (Just inexps))
        :[]
        ) =
    let
        macros2 = add macros macroname macro
        macro ctx eval es =
            let env2 = nestEnv (M.fromList [(paramname, es)]) env
            in evalFun (Ctx tb env2 macros2) bodyexps
        add macros name macro = M.insert name macro macros
    in
        evalFun (Ctx tb env macros2) inexps
def _ _ e = error "bad usage of macro def"

foreach :: Ctx -> (Ctx -> [Exp] -> IO [Exp]) -> [Exp] -> IO [Exp]
foreach ctx@(Ctx tb env funs) evalFun
    (
        listExp
        :(Str varname)
        :(Tag (Str "") (M.toList -> []) (Just targ))
        :[]
        ) =
    let
        aux varname item = do
            let env2 = nestEnv env (M.fromList [(varname, [item])])
            evalFun (Ctx tb env2 funs) targ
    in do
        list <- evalFun ctx [listExp]
        (case list of
            [Tag (Str "") (M.toList -> []) (Just xs)]
                -> concatMapM (aux varname) xs
            _   -> error "expression passed to foreach does not evaluate to a list"
            )
foreach _ _ e = error ("bad usage of macro foreach:" ++ (show e))

concatMacro :: [Exp] -> IO [Exp]
concatMacro [Tag (Str "") (M.toList -> []) (Just xs)]
    | allStrings    = return . (:[]) . Str . concat $ bareStrings
    | otherwise     = error $ "concat called on list with non-string element: "
    where
        allStrings  = (foldr (&&) True) . (map isStr) $ xs
        bareStrings = map unwrap1Str xs
concatMacro _ = error "concat called on non-list"

importscripts :: Ctx -> (Ctx -> [Exp] -> IO [Exp]) -> [Exp] -> IO [Exp]
importscripts ctx@(Ctx {ctxMacros = macros}) eval
    ((Tag (Str "") _ (Just nameexps)):(Tag (Str "") _ (Just exps)):[]) = do
    nameexps2 <- eval ctx nameexps
    let names = map unwrap1Str nameexps2
    let addScript macros name =
            M.insert name (strictIO $ runScript name) macros
    let macros2 = foldl addScript macros names

    eval (ctx {ctxMacros = macros2}) exps
importscripts _ _ _ = error "bad usage of importscripts"

strictIO f ctx eval xs = (eval ctx xs) >>= f
strict f ctx eval xs = (eval ctx xs) >>= return . f

funs :: MacroFuns
funs = M.fromList [
    ("prettyfilesize", strict prettyfilesize),
    ("filesize", strictIO filesize),
    ("def", def),
    ("case", caseMacro),
    ("foreach", foreach),
    ("inc", strictIO inc),
    ("rawinc", strictIO rawinc),
    ("concat", strictIO concatMacro),
    ("importscripts", importscripts)
    ]

