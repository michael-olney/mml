{-# LANGUAGE FlexibleContexts, TupleSections #-}

module MML.Lex (
    tokenize,
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

import MML.Eval

tokenize :: String -> String -> IO (Either String [TokenPos])
tokenize name mml = let
    r = P.parse tokens name mml
    in
        (case r of
            (Left err)      -> return . Left . show $ err
            (Right xs)      -> return . Right $ xs
            )

data BraceVariant = BVSpecialLike | BVCharLike
    deriving (Show, Eq, Ord)
data BraceDir = BDOpen | BDClose
    deriving (Show, Eq, Ord)
data BraceType = BTTag | BTCall | BTVar | BTUnknown
    deriving (Show, Eq, Ord)
data SpaceType = STBare | STEscaped
    deriving (Show, Eq, Ord)

data Token =
    TChar Char
    | TSpace (Maybe Char)
    | TBrace BraceType BraceDir BraceVariant
    | TEmptyStr
    | TStrSep
    | TSplit
    | TEOF
    deriving (Show, Eq, Ord)

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

isChar (TChar {})               = True
isChar x                        = False

data GroupType a =
    GTSpace a | GTChar a | GTSpecial a | GTCharLike BraceDir a
    deriving (Show, Eq)

unwrapGT (GTSpace xs)           = xs
unwrapGT (GTChar xs)            = xs
unwrapGT (GTCharLike _ xs)      = xs
unwrapGT (GTSpecial xs)         = xs

eatspaces :: [TokenPos] -> [TokenPos]
eatspaces xs = eatruns runs
    where
        runs = (map aux) . (groupBy groupFun2) $ xs
        groupFun2 x y = aux2 x == aux2 y
        isCharLikeO (TBrace _ BDOpen BVCharLike)    = True
        isCharLikeO _                               = False
        isCharLikeC (TBrace _ BDClose BVCharLike)   = True
        isCharLikeC _                               = False
        aux x   | isSpace . fst . head $ x      = GTSpace x
                | isChar . fst . head $ x       = GTChar x
                | isCharLikeO . fst . head $ x  = (GTCharLike BDOpen) x
                | isCharLikeC . fst . head $ x  = (GTCharLike BDClose) x
                | otherwise                     = GTSpecial x
        aux2 x  | isSpace . fst $ x             = GTSpace ()
                | isChar . fst $ x              = GTChar ()
                | isCharLikeO . fst $ x         = GTCharLike BDOpen ()
                | isCharLikeC . fst $ x         = GTCharLike BDClose ()
                | otherwise                     = GTSpecial ()
        eatruns ((GTChar x):(GTSpace s):(GTChar y):xs) =
            (x ++ (one s)) ++ (eatruns $ (GTChar y):xs)
        eatruns ((GTCharLike BDClose x):(GTSpace s):(GTChar y):xs) =
            (x ++ (one s)) ++ (eatruns $ (GTChar y):xs)
        eatruns ((GTChar x):(GTSpace s):(GTCharLike BDOpen y):xs) =
            (x ++ (one s)) ++ (eatruns $ (GTCharLike BDOpen y):xs)
        eatruns ((GTCharLike BDClose x):(GTSpace s):(GTCharLike BDOpen y):xs) =
            (x ++ (one s)) ++ (eatruns $ (GTChar y):xs)
        eatruns (x:(GTSpace s):xs) =
            ((unwrapGT x) ++ (all s)) ++ (eatruns xs)
        eatruns ((GTSpace s):xs) =
            (all s) ++ (eatruns xs)
        eatruns (x:xs) =
            (concatMap unwrapGT [x]) ++ (eatruns xs)
        eatruns [] = []
        all = filter (isEscapedSpace . fst)
        one [] = []
        one xs@((_, pos):_) | (length . all $ xs) > 0 = all xs
                            | otherwise =
                ((TSpace Nothing, pos):) . (filter (isEscapedSpace . fst)) $ xs

tokens :: Parser [TokenPos]
tokens = rawTokens >>= return . eatspaces

special = "<>{}:\\~%$^"
whitespace = " \x0d\x0a\t"

