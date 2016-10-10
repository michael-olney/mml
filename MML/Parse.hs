{-# LANGUAGE FlexibleContexts #-}

module MML.Parse (parse) where

import Prelude hiding (exp)
import Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec.Prim hiding (parse)
import Control.Monad
import Data.List

--import qualified MML.Parsev0 as V0

import MML.Types
import MML.Eval

parse :: String -> String -> IO (Either String Doc)
parse name mml = let
    r = P.parse doc name mml
    in
        (case r of
            (Left err)  -> return . Left . show $ err
            (Right x)   -> return . Right $ x
            )

special = "<>{}:\\~%$"
whitespace = " \x0d\x0a\t"

escapable s = do
    do
        char '\\'
        anyChar
    <|> noneOf s

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
    <|> (do { r <- (call <|> var <|> tag); whitespaces; return r})

exps :: Parser [Exp]
exps = do
    whitespaces
    many (do { r <- exp; whitespaces; return r; })

attr :: Parser (Exp, [Exp])
attr = do
    string "<"
    whitespaces
    name <- exp
    string ":"
    val <- exps
    string ">"
    return (name, val)

sourcePos :: Parser SourcePos
sourcePos = liftM statePos getParserState

traceback :: String -> Parser TracebackRecord
traceback macroname = do
    sp <- sourcePos
    let name = sourceName sp
    let line = sourceLine sp
    let col = sourceColumn sp
    return $ (TracebackRecord name line col macroname)

var :: Parser Exp
var = do
    try $ string  "<$"
    whitespaces
    x <- exp
    name <- (case x of
            (Str name)  -> return name
            _           -> fail "variable name must be STRING"
            )
    string ">"
    return (Var name)

call :: Parser Exp
call = do
    try $ string  "<%"
    whitespaces
    name <- exp
    tb <- (case name of
            (Str name)  -> traceback name
            _           -> traceback "<unknown macro>"
            )
    string ":"
    cs <- exps
    string ">"
    return (Call tb name cs)

tag :: Parser Exp
tag = do
    string "<"
    whitespaces
    name <- exp
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
str =
    (do
        string "%"
        whitespaces
        return . Str $ "")
    <|> (do
        xs <- many1 (word <|> gap)
        let ss = concat . (map dropEither) . rmTrailing . (map collapseSpaces) $ xs
        return . Str $ ss)
