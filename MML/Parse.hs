{-# LANGUAGE FlexibleContexts #-}

module MML.Parse (parse) where

import Prelude hiding (exp)
import Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec.Prim hiding (parse)
import Control.Monad
import Data.List

import qualified MML.Parsev0 as V0

import MML.Types
import MML.Eval

parse :: Params -> MacroFuns -> String -> String -> IO (Either String Doc)
parse params funs name mml = let
    r = P.parse doc name mml
    in
        (case r of
            (Left err)  -> return . Left . show $ err
            (Right x)   -> return . Right $ x
            )

special = "<>{}:\\~"
whitespace = " \x0d\x0a\t"

escapable s = do
    do
        char '\\'
        anyChar
    <|> noneOf s

nameChar = escapable (special ++ whitespace)

ordinary :: Parser Char
ordinary = escapable special


whitespaces :: Parser String
whitespaces = many . oneOf $ whitespace

doc :: Parser Doc
doc = do
    cs <- exps
    eof
    return cs

exp :: Parser Exp
exp =
    (do { r <- str; optional (do { string "~"; whitespaces}); return r })
    <|> (do { r <- (tag <|> meta); whitespaces; return r})

exps :: Parser [Exp]
exps = do
    whitespaces
    many (do { r <- exp; whitespaces; return r; })

attr :: Parser (String, [Exp])
attr = do
    string "<"
    whitespaces
    name <- many nameChar
    whitespaces
    string ":"
    val <- exps
    string ">"
    return (name, val)

meta :: Parser Exp
meta = do
    string "{"
    whitespaces
    name <- many nameChar
    whitespaces
    string ":"
    cs <- exps
    string "}"
    return (Call name cs)

tag :: Parser Exp
tag = do
    string "<"
    whitespaces
    x <- exp
    name <- case x of
        (Str name) -> return name
        _ -> fail "name must be STRING value"
    whitespaces
    attrs <- endBy attr whitespaces
    exp <- optionMaybe tagExps
    string ">"
    return (Tag name attrs exp)

tagExps :: Parser [Exp]
tagExps = do
    string ":"
    exps

collapseSpaces :: Either String String -> Either String String
collapseSpaces e@(Left _)                                   = e
collapseSpaces e@(Right (x:xs))     | elem x whitespace     = Right " "
                                    | otherwise             = e
rmTrailing :: [Either String String] -> [Either String String]
rmTrailing []                                       = []
rmTrailing xs@(_:_)     | last xs == (Right " ")    = take (length xs - 1) xs
                        | otherwise                 = xs

dropEither :: Either a a -> a
dropEither (Left x)     = x
dropEither (Right x)    = x

word :: Parser (Either String String)
word = do
    r <- many1 (escapable (special ++ whitespace))
    return . Left $ r

gap :: Parser (Either String String)
gap = do
    r <- many1 . oneOf $ whitespace
    return . Right $ r

str :: Parser Exp
str = do
    xs <- many1 (word <|> gap)
    let ss = concat . (map dropEither) . rmTrailing . (map collapseSpaces) $ xs
    return . Str $ ss
