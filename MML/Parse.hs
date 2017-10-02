{-# LANGUAGE FlexibleContexts, TupleSections #-}

module MML.Parse (parse) where

import MML.Types
import MML.Lex

import Prelude hiding (exp)

import Text.Parsec (Parsec)
import Text.ParserCombinators.Parsec hiding (parse, token, tokens, Parser)
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec.Prim hiding (parse, token, tokens, Parser)

import Control.Monad

import Data.List
import qualified Data.Map as M

type Parser a = Parsec [TokenPos] () a

sourceLoc :: Parser SourceLoc
sourceLoc = do
    sp <- sourcePos <?> "source position"
    let name = sourceName sp
    let line = sourceLine sp
    let col = sourceColumn sp
    return $ (SourceLoc name line col)

parse :: String -> String -> IO (Either String Doc)
parse name mml = do
    toksr <- tokenize name mml
    (case toksr of
            (Left x)        -> return . Left $ x
            (Right toks)    -> do
                let r = P.parse doc name toks
                (case r of
                        (Left err)  -> return . Left . show $ err
                        (Right x)   -> return . Right $ x
                        )
            )

token :: (Token -> Bool) -> Parser Token
token p = tokenPrim show nextPos testToken
    where
        nextPos _ _ ((_, pos):_)    = pos
        nextPos pos _ []            = pos
        testToken (x, pos)          = if p x then Just x else Nothing

doc :: Parser Doc
doc = do
    cs <- exps
    token (== TEOF)
    return cs

exp :: Parser Exp
exp =
    do
        r <- str
        optional $ token (== TStrSep)
        return r
    <|> tag

exps :: Parser [Exp]
exps = many exp

matchTagBrace (TBrace BTTag BDOpen bv_ )    = True
matchTagBrace _                             = False

name :: String -> Parser String
name nameType = do
    nameExp <- exp
    case nameExp of
        (Str nameStr _) -> return nameStr
        _               -> fail $ "expected " ++ nameType ++ ", got expression"

attr :: Parser (String, [Exp])
attr = do
    (TBrace BTTag BDOpen bv) <- token matchTagBrace
    attrName <- name "attribute name"

    token (== TSplit) <?> "attribute split"
    val <- exps
    token (== (TBrace BTUnknown BDClose bv))
    return (attrName, val)

sourcePos :: Parser SourcePos
sourcePos = liftM statePos getParserState

tag :: Parser Exp
tag = do
    (TBrace BTTag BDOpen bv) <- token matchTagBrace

    nameSL <- sourceLoc
    tagName <- name "tag name"

    attrsSL <- sourceLoc
    attrs <- many attr >>= return . M.fromList

    childrenSL <- sourceLoc
    exp <- optionMaybe tagExps

    token (== (TBrace BTUnknown BDClose bv))

    let sm = SMTag {
                smTagName = nameSL,
                smTagAttrs = attrsSL,
                smTagChildren = childrenSL
            }

    return (Tag tagName attrs exp sm)

tagExps :: Parser [Exp]
tagExps = do
    token (== TSplit)
    exps

matchBrace (TChar _)    = True
matchBrace _            = False

matchBareSpace (TSpace Nothing) = True
matchBareSpace _                = False

matchEscapedSpace (TSpace (Just _)) = True
matchEscapedSpace _                 = False

strChar :: Parser Char
strChar =
    do
        (TChar x) <- token matchBrace
        return x
    <|> do
        (TSpace Nothing) <- token matchBareSpace
        return ' '
    <|> do
        (TSpace (Just x)) <- token matchEscapedSpace
        return x

str :: Parser Exp
str =
    do
        sl <- sourceLoc
        token (== TEmptyStr)
        return $ Str "" (SMStr sl)
    <|> do
        sl <- sourceLoc
        cs <- many1 strChar
        return $ Str cs (SMStr sl)

