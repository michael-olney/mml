{-# LANGUAGE FlexibleContexts, TupleSections, DeriveDataTypeable #-}

module MML.Lex (
    tokenize,
    TokenPos, Token(..),
    BraceDir(..),
    isSpace
    ) where

import Prelude hiding (exp)

import Text.ParserCombinators.Parsec hiding (parse, tokens, space)
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
    | TSpace
    | TBrace BraceDir
    | TSplit
    | TEOF
    deriving (Show, Eq, Ord, Data)

isChar (TChar _)                    = True
isChar _                            = False

isSpace TSpace  = True
isSpace _       = False

type TokenPos = (Token, SourcePos)

single str con = do
    pos <- getPosition
    string str
    return . (, pos) $ con

braces = single "{"  (TBrace BDOpen ) <|> single "}"  (TBrace BDClose)

splits = try (single "->"  TSplit)

space = do
    pos <- getPosition
    oneOf whitespaceChars >> return (TSpace, pos)

slashedChar = do
    pos <- getPosition
    string "\\"
    anyChar >>= return . (, pos) . TChar

bareChar = do
    pos <- getPosition
    anyChar >>= return . (, pos) . TChar

nbsp = do
    pos <- getPosition
    string "~" >> return (TChar '\x00a0', pos)

basicToken :: Parser TokenPos
basicToken =
    braces
    <|> splits
    <|> space
    <|> nbsp
    <|> slashedChar
    <|> bareChar

extTokens :: Parser [TokenPos]
extTokens = do
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
        tok <- basicToken
        return [tok]

tokens :: Parser [TokenPos]
tokens = do
    xs <- many extTokens
    pos <- getPosition
    eof
    return . (++ [(TEOF, pos)]) $ concat xs

whitespaceChars = " \x0d\x0a\t"

