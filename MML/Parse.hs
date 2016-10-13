{-# LANGUAGE FlexibleContexts, TupleSections #-}

module MML.Parse (
    parse, tokenize,
    TokenPos, Token(..),
    BraceVariant(..),
    BraceDir(..),
    BraceType(..),
    SpaceType(..)
    ) where

import Prelude hiding (exp)
import Text.ParserCombinators.Parsec hiding (parse, tokens)
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec.Prim hiding (parse, tokens)
import Control.Monad
import Data.List
import Debug.Trace

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

tokenize :: String -> String -> IO (Either String [Token])
tokenize name mml = let
    r = P.parse tokens name mml
    in
        (case r of
            (Left err)      -> return . Left . show $ err
            (Right xs)      -> return . Right . fst . unzip $ xs
            )

data BraceVariant = BVSpecialLike | BVCharLike
    deriving (Show, Eq)
data BraceDir = BDOpen | BDClose
    deriving (Show, Eq)
data BraceType = BTTag | BTCall | BTVar | BTUnknown
    deriving (Show, Eq)
data SpaceType = STBare | STEscaped
    deriving (Show, Eq)

data Token =
    TChar Char
    | TSpace (Maybe Char)
    | TBrace BraceType BraceDir BraceVariant
    | TEmptyStr
    | TStrSep
    | TSplit
    | TEOF
    deriving (Show, Eq)

type TokenPos = (Token, SourcePos)

brace str bt bd bv = do
    pos <- getPosition
    try $ string str
    return . (, pos) $ TBrace bt bd bv

single str con = do
    pos <- getPosition
    string str
    return . (, pos) $ con

rawToken :: Parser TokenPos
rawToken = do
    do
        pos <- getPosition
        oneOf whitespace
        return . (, pos) $ TSpace Nothing
    <|> brace "<%"  BTCall      BDOpen  BVSpecialLike
    <|> brace "<$"  BTVar       BDOpen  BVSpecialLike
    <|> brace "<"   BTTag       BDOpen  BVSpecialLike
    <|> brace ">"   BTUnknown   BDClose BVSpecialLike
    <|> brace "{%"  BTCall      BDOpen  BVCharLike
    <|> brace "{$"  BTVar       BDOpen  BVCharLike
    <|> brace "{"   BTTag       BDOpen  BVCharLike
    <|> brace "}"   BTUnknown   BDClose BVCharLike
    <|> single "^"  TEmptyStr
    <|> single "~"  TStrSep
    <|> single ":"  TSplit
    <|> do
        pos <- getPosition
        x <- noneOf (whitespace ++ special)
        return . (, pos) . TChar $ x
    <|> try (do
        pos <- getPosition
        string "\\"
        x <- noneOf whitespace
        return . (, pos) . TChar $ x)
    <|> try (do
        pos <- getPosition
        string "\\"
        x <- oneOf whitespace
        return . (, pos) . TSpace . Just $ x)

rawTokens :: Parser [TokenPos]
rawTokens = do
    xs <- many rawToken
    pos <- getPosition
    eof
    return . (++ [(TEOF, pos)]) $ xs

isSpace (TSpace _)  = True
isSpace x           = False

isEscapedSpace (TSpace (Just _))    = True
isEscapedSpace _                    = False

isChar (TBrace _ _ BVCharLike)  = True
isChar (TChar {})               = True
isChar x                        = False

data GroupType = GTSpace [TokenPos] | GTChar [TokenPos] | GTSpecial [TokenPos]
    deriving (Show, Eq)

unwrapGT (GTSpace xs)   = xs
unwrapGT (GTChar xs)    = xs
unwrapGT (GTSpecial xs) = xs

eatspaces :: [TokenPos] -> [TokenPos]
eatspaces xs = eatruns runs
    where
        runs = (map aux) . (groupBy groupFun2) $ xs
        groupFun = isSpace . fst
        groupFun2 x y = groupFun x == groupFun y
        aux x   | isSpace . fst . head $ x  = GTSpace x
                | isChar . fst . head $ x   = GTChar x
                | otherwise                 = GTSpecial x
        eatruns ((GTChar x):(GTSpace s):(GTChar y):xs) =
            (x ++ (one s)) ++ (eatruns $ (GTChar y):xs)
        eatruns (x:(GTSpace s):xs) =
            ((unwrapGT x) ++ (all s)) ++ (eatruns xs)
        eatruns ((GTSpace s):xs) =
            (all s) ++ (eatruns xs)
        eatruns x =
            concatMap unwrapGT x
        all = filter (isEscapedSpace . fst)
        one [] = []
        one xs@((_, pos):_) | (length . all $ xs) > 0 = all xs
                            | otherwise =
                ((TSpace Nothing, pos):) . (filter (isEscapedSpace . fst)) $ xs

tokens :: Parser [TokenPos]
tokens = rawTokens >>= return . eatspaces

special = "<>{}:\\~%$^"
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
    do
        r <- str
        optional (do { string "~"; whitespaces})
        return r
    <|> do
        r <- (call <|> var <|> tag)
        whitespaces
        return r

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
    do
        string "^"
        whitespaces
        return . Str $ ""
    <|> do
        xs <- many1 (word <|> gap)
        let ss = concat . (map dropEither) . rmTrailing . (map collapseSpaces) $ xs
        return . Str $ ss

