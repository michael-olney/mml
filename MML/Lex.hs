{-# LANGUAGE FlexibleContexts, TupleSections, DeriveDataTypeable #-}

module MML.Lex (
    tokenize,
    TokenPos, Token(..),
    BraceDir(..),
    ) where

import Prelude hiding (exp)

import Text.ParserCombinators.Parsec hiding (parse, tokens)
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec.Prim hiding (parse, tokens)
import Control.Monad

import Data.Data
import Data.List

tokenize :: String -> String -> IO (Either String [TokenPos])
tokenize name mml = let
    r = P.parse tokens name mml
    in
        (case r of
            (Left err)      -> return . Left . show $ err
            (Right xs)      -> return . Right $ xs
            )

data BraceDir = BDOpen | BDClose
    deriving (Show, Eq, Ord, Data)

data Token =
    TChar Char
    | TSpace (Maybe Char)
    | TBrace BraceDir
    | TSplit
    | TEOF
    deriving (Show, Eq, Ord, Data)

isChar (TChar _)                    = True
isChar _                            = False

isBareSpace (TSpace Nothing)        = True
isBareSpace _                       = False

isCoveredSpace (TSpace (Just _))    = True
isCoveredSpace _                    = False

type TokenPos = (Token, SourcePos)

brace str bd = do
    pos <- getPosition
    try $ string str
    return . (, pos) $ TBrace bd

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
    <|> brace "{"  BDOpen 
    <|> brace "}"  BDClose
    <|> single "→"  TSplit
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


tokensAux :: Parser [TokenPos]
tokensAux = do
    do
        pos <- getPosition
        try $ string "/*"
        manyTill anyChar (string "*/")
        return []
    <|> do
        pos <- getPosition
        string "`"
        toks <- many $ do
            pos <- getPosition
            tok <- noneOf "`"
            return (TChar tok, pos)
        string "`"
        return toks
    <|> do
        tok <- rawToken
        return [tok]

tokens :: Parser [TokenPos]
tokens = do
    xs <- many tokensAux
    pos <- getPosition
    eof
    return . (++ [(TEOF, pos)]) $ concat xs

special = "{}→\\~^`"
whitespace = " \x0d\x0a\t"

