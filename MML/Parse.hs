{-#
LANGUAGE FlexibleContexts, TupleSections, ScopedTypeVariables,
ViewPatterns
#-}

module MML.Parse (parse) where

import MML.Types
import MML.Lex

import Prelude hiding (exp)

import Text.Parsec (Parsec)
import Text.ParserCombinators.Parsec hiding (parse, token, tokens, Parser)
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec.Prim hiding (parse, token, tokens, Parser)

import Control.Monad
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class

import Data.Generics

import Data.List
import qualified Data.Map as M

type Parser a = Parsec [TokenPos] () a

type TokenExp = ExpAux Token
type TokenAttrMap = AttrMapAux Token
type TokenExpList = ExpListAux Token

sourceLoc :: Parser SourceLoc
sourceLoc = do
    sp <- sourcePos <?> "source position"
    let name = sourceName sp
    let line = sourceLine sp
    let col = sourceColumn sp
    return $ (SourceLoc name line col)

elimEmptyStrs :: [TokenExp] -> [TokenExp]
elimEmptyStrs = everywhere $ mkT (ExpList . elimFlat . unwrapExpList)
    where
    elimFlat = filter (not . isEmptyStr)
    isEmptyStr (Str (filter (not . isBareSpace) -> []) _)   = True
    isEmptyStr _                                            = False

reduceSpaces :: [TokenExp] -> [TokenExp]
reduceSpaces = everywhere $ mkT red
    where
        red (sp@(TSpace Nothing):(TSpace Nothing):es)   = sp:es
        red e                                           = e

elimSpaces :: [TokenExp] -> [TokenExp]
elimSpaces = everywhere $ mkT elimSpacesFlat

elimSpacesFlat :: TokenExpList -> TokenExpList
elimSpacesFlat = ExpList . elim . unwrapExpList
    where
        elim []                     = []
        elim e@(head -> Str ts sm)  = [Str (elimLeft ts) sm] ++ (tail e)
        elim e@(last -> Str ts sm)  = (init e) ++ [Str (elimRight ts) sm]
        elim e                      = e
        elimLeft                    = dropWhile isBareSpace
        elimRight                   = dropWhileEnd isBareSpace

tokToChar :: Token -> Char
tokToChar (TChar c)          = c
tokToChar (TSpace Nothing)   = ' '
tokToChar (TBrace BDOpen)    = '{'
tokToChar (TBrace BDClose)   = '}'
tokToChar TSplit             = '→'

convertTokens :: TokenExp -> Exp
convertTokens = fmap tokToChar

postproc :: [TokenExp] -> [Exp]
postproc = (map convertTokens) . elimEmptyStrs . elimSpaces . reduceSpaces

parse :: String -> String -> IO (Either String Doc)
parse name mml = runEitherT $ do
    toks <- EitherT $ tokenize name mml
    exps <- ppError $ P.parse doc name toks
    return . postproc $ exps
    where
        ppError (Left err)  = left . show $ err
        ppError (Right res) = right res

token :: (Token -> Bool) -> Parser Token
token p = tokenPrim show nextPos testToken
    where
        nextPos _ _ ((_, pos):_)    = pos
        nextPos pos _ []            = pos
        testToken (x, pos)          = if p x then Just x else Nothing

doc :: Parser [TokenExp]
doc = do
    cs <- many exp
    token (== TEOF)
    return cs

exp :: Parser TokenExp
exp = str <|> tag

name :: String -> Parser String
name nameType = (many (token isChar) <?> nameType) >>= return . map tokToChar

bareSpaces :: Parser [Token]
bareSpaces = many $ token isBareSpace

attr :: Parser (String, [TokenExp])
attr = do
    token (== TBrace BDOpen)
    bareSpaces

    attrName <- name "attribute name"
    bareSpaces

    token (== TSplit) <?> "attribute split"
    val <- many exp
    token (== TBrace BDClose)
    bareSpaces
    return (attrName, val)

sourcePos :: Parser SourcePos
sourcePos = liftM statePos getParserState

tag :: Parser TokenExp
tag = do
    token (== TBrace BDOpen)
    bareSpaces

    nameSL <- sourceLoc
    tagName <- name "tag name"
    bareSpaces

    attrsSL <- sourceLoc
    attrs <- many attr >>= return . (M.map ExpList) . M.fromList
    bareSpaces

    childrenSL <- sourceLoc
    exp <- optionMaybe tagExps

    token (== TBrace BDClose)

    let sm = SMTag {
                smTagName = nameSL,
                smTagAttrs = attrsSL,
                smTagChildren = childrenSL
            }

    return $ Tag tagName attrs (exp >>= (Just . ExpList)) sm

tagExps :: Parser [TokenExp]
tagExps = token (== TSplit) >> many exp

strChar :: Parser Token
strChar =
    token isChar
    <|> token isBareSpace
    <|> token isCoveredSpace

str :: Parser TokenExp
str = do
    sl <- sourceLoc
    cs <- many1 strChar
    return $ Str cs (SMStr sl)

isChar (TChar _)                    = True
isChar _                            = False

isBareSpace (TSpace Nothing)        = True
isBareSpace _                       = False

isCoveredSpace (TSpace (Just _))    = True
isCoveredSpace _                    = False
