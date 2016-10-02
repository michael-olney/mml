{-# LANGUAGE FlexibleContexts #-}

module MML (
    parseMML,
    module MML.Types,
    module MML.HTML
    ) where

import MML.Types
import MML.HTML
import Text.ParserCombinators.Parsec
import qualified Data.Map as M
import Control.Monad

{-
Syntax TODO:
1) Eat more whitespace
2) Change {} to <%>
3) Introduce variables ==> <$> (pass env to macros)
4) Change colon to another character
5) Add version specifier to head!
6) Allow empty strings to be expressed
-}

{-
Other TODO:
1) Split syntax, semantics stuff into different modules
2) Allow macros to be defined by external executables
3) Improve traceback for failed macros
4) Build macros for building new macros - ensure this is modular!
5) Dependency analysis
-}

eaters = "<:>~%"
special = eaters ++ "\\"
whitespace = " \x0d\x0a\t"

escapable s = do
    do
        char '\\'
        anyChar
    <|> noneOf s

nameChar = escapable (special ++ whitespace)
ordinary = escapable special

whitespaces = many . oneOf $ whitespace

-- TODO detect macro returning macro here
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

run :: Params -> MacroFuns -> Doc -> IO Doc
run params funs (Doc cs) = (runConts params funs cs) >>= return . Doc . collapse

parseMML :: Params -> MacroFuns -> String -> String -> IO Doc
parseMML params funs name mml = let
    r = parse doc name mml
    in
        (case r of
            (Left err)  -> error . show $ err
            (Right doc) -> run params funs doc)

conts :: Parser [Cont]
conts = do
    many1 (meta <|> tag <|> str)
    <|> return []

doc :: Parser Doc
doc = do
    cs <- conts
    whitespaces
    eof
    return (Doc cs)

cont :: Parser Cont
cont = tag <|> meta <|> str

attr :: Parser (String, [Cont])
attr = do
    string "<"
    whitespaces
    name <- many nameChar
    whitespaces
    string ":"
    val <- many cont
    string ">"
    return (name, val)

meta :: Parser Cont
meta = do
    string "{"
    whitespaces
    name <- many nameChar
    whitespaces
    string ":"
    cs <- conts
    whitespaces
    string "}"
    return (Macro name cs)

tag :: Parser Cont
tag = do
    string "<"
    whitespaces
    name <- many nameChar
    whitespaces
    attrs <- endBy attr whitespaces
    whitespaces
    cont <- optionMaybe tagConts
    whitespaces
    string ">"
    return (Tag name attrs cont)

tagConts :: Parser [Cont]
tagConts = do
    string ":"
    many cont

str :: Parser Cont
str = do
    ss <- many1 ordinary
    return (Str ss)

