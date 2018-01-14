{-#
LANGUAGE    FlexibleContexts, TupleSections, ScopedTypeVariables,
            ViewPatterns
#-}

module MML.Parse (parse, isEmptyIntermItem, elimMiddle, elimSides) where

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

import Data.List
import Data.Either
import qualified Data.Map as M
import Data.Generics

type Parser a = Parsec [TokenPos] () a

sourceLoc :: Parser SourceLoc
sourceLoc = do
    sp <- sourcePos <?> "source position"
    let name = sourceName sp
    let line = sourceLine sp
    let col = sourceColumn sp
    return $ (SourceLoc name line col)

----------------------------
--- Whitespace Reduction ---
----------------------------

-- Intermediate form used for whitespace reduction.

type ElimIntermItem = Either (Token, SourceMap) TokenExp
type ElimInterm = [ElimIntermItem]

isEmptyIntermItem  :: ElimIntermItem -> Bool
isEmptyIntermItem = (== 0) . (gcount $ mkQ False query) . clearAttrs
    where
        query (TChar _)         = True
        query (TSpace (Just _)) = True
        query e                 = False
        clearAttrs :: ElimIntermItem -> ElimIntermItem
        clearAttrs = everywhere $ mkT clearAttrsT
        clearAttrsT (m::TokenAttrMap) = M.empty

toElimInterm :: [TokenExp] -> ElimInterm
toElimInterm = concatMap aux
    where
        aux (Str ts sm) = map Left $ zip ts (cycle [sm])
        aux e           = [Right e]

unwrapL (Left x) = x

fromElimInterm :: ElimInterm -> [TokenExp]
fromElimInterm [] = []
fromElimInterm interm@((isLeft -> True):_) =
    let
        sm = snd . unwrapL . head $ interm
        prefix = [Str (map (fst . unwrapL) $ takeWhile isLeft $ interm) sm]
        suffix = dropWhile isLeft interm
    in
        prefix ++ (fromElimInterm suffix)
fromElimInterm interm@((isRight -> True):_) =
    [unwrap $ head interm] ++ (fromElimInterm $ tail interm)
        where unwrap (Right x) = x

-- Whitespace reduction

elimSpaces :: [TokenExp] -> [TokenExp]
elimSpaces = unwrapExpList . (everywhere $ mkT elimSpacesFlat) . ExpList

elimSpacesFlat :: TokenExpList -> TokenExpList
elimSpacesFlat = post . elim . pre
    where
        pre = toElimInterm . unwrapExpList
        post = ExpList . fromElimInterm
        elim = elimSides . elimMiddle

elimMiddle :: ElimInterm -> ElimInterm
elimMiddle = concatMap elim  . groupBy (\x y -> gpred x == gpred y)
    where
        gpred = isEmptyIntermItem

        elim [] = []
        elim run@((isEmptyIntermItem -> True):_)
            | spaces /= []  = head spaces : suffix
            | otherwise     = suffix
            where
                spaces = filter isBareSpaceInterm run
                suffix = filter (not . isBareSpaceInterm) run
        elim run = run

elimSides :: ElimInterm -> ElimInterm
elimSides = elimLeft . elimRight
    where
        elimLeft = dropWhile isBareSpaceInterm
        elimRight = reverse . elimLeft . reverse

isBareSpaceInterm (Left (x, _)) = isBareSpace x
isBareSpaceInterm _             = False

-------------------------------
--- Main Parser Definitions ---
-------------------------------

-- Top-Level Parser
doc :: Parser [TokenExp]
doc = do
    cs <- many exp
    token (== TEOF)
    return cs

token :: (Token -> Bool) -> Parser Token
token p = tokenPrim show nextPos testToken
    where
        nextPos _ _ ((_, pos):_)    = pos
        nextPos pos _ []            = pos
        testToken (x, pos)          = if p x then Just x else Nothing

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

-----------------------------------------
--- Parser Runner and Post-Processing ---
-----------------------------------------

-- Whitespace handling requires tokens in order to distinguish
-- between escaped and bare whitespace.

type TokenExp = ExpAux Token
type TokenAttrMap = AttrMapAux Token
type TokenExpList = ExpListAux Token

tokToChar :: Token -> Char
tokToChar (TChar c)          = c
tokToChar (TSpace Nothing)   = ' '
tokToChar (TBrace BDOpen)    = '{'
tokToChar (TBrace BDClose)   = '}'
tokToChar TSplit             = '→'

convertTokens :: TokenExp -> Exp
convertTokens = fmap tokToChar

postproc :: [TokenExp] -> [Exp]
postproc = (map convertTokens) . elimSpaces

parse :: String -> String -> IO (Either String Doc)
parse name mml = runEitherT $ do
    toks <- EitherT $ tokenize name mml
    exps <- ppError $ P.parse doc name toks
    return . postproc $ exps
    where
        ppError (Left err)  = left . show $ err
        ppError (Right res) = right res