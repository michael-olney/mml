{-#
LANGUAGE    FlexibleContexts, TupleSections, ScopedTypeVariables,
            ViewPatterns
#-}

module MML.Parse (parse) where

import MML.Types
import MML.Lex

import Prelude hiding (exp)

import Text.Parsec (Parsec)
import Text.ParserCombinators.Parsec hiding (parse, token, tokens, Parser, spaces)
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec.Prim hiding (parse, token, tokens, Parser)

import Control.Monad
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class

import Data.List
import Data.Either
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Generics

type Parser a = Parsec [TokenPos] () a

sourceLoc :: Parser SourceLoc
sourceLoc = do
    sp <- sourcePos <?> "source position"
    let name = sourceName sp
    let line = sourceLine sp
    let col = sourceColumn sp
    return $ (SourceLoc name line col)

phrasingTags = S.fromList [
    "a", "abbr", "audio", "b", "bdi", "bdo",
    "br", "button", "cite", "code", "data",
    "datalist", "del", "em", "i", "ins",
    "kbd", "mark", "math", "meter",
    "output", "q", "ruby", "s", "samp",
    "small", "span", "strong", "sub", "sup",
    "time", "u", "var", "wbr"
    ]

isPhrasing :: String -> Bool
isPhrasing x = S.member x phrasingTags

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
        query e                 = False
        clearAttrs :: ElimIntermItem -> ElimIntermItem
        clearAttrs = everywhere $ mkT clearAttrsT
        clearAttrsT (m::TokenAttrMap) = M.empty

isPhrasingIntermItem :: ElimIntermItem -> Bool
isPhrasingIntermItem (Right (Tag (isPhrasing -> True) _ _ _))   = True
isPhrasingIntermItem (Left (TChar _, _))                        = True
isPhrasingIntermItem _                                          = False

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

tripleMap ::  (a -> a -> a -> a) -> [a] -> [a]
tripleMap f (x:y:z:rest) = x : tripleMap f (f x y z:z:rest)
tripleMap f x = x

data ElimClass = Space | Phras | Other
    deriving (Eq, Show, Ord)

elimMiddle :: ElimInterm -> ElimInterm
elimMiddle = concat
                . tripleMap elim
                . groupBy (\x y -> classify x == classify y)
    where
        classify (isSpaceIntermItem -> True)    = Space
        classify (isPhrasingIntermItem -> True) = Phras
        classify _                              = Other

        classifyHead (x:_)  = classify x
        classifyHead _      = Space

        ch = classifyHead

        elim :: ElimInterm -> ElimInterm -> ElimInterm -> ElimInterm
        elim (ch -> Space) (ch -> Space)     _             = error "internal"
        elim _             (ch -> Space)     (ch -> Space) = error "internal"
        elim (ch -> Other) (ch -> Space)     _             = []
        elim _             (ch -> Space)     (ch -> Other) = []
        elim (ch -> Phras) run@(ch -> Space) (ch -> Phras) = collapse run
        elim _             x                 _             = x

        collapse :: ElimInterm -> ElimInterm
        collapse []  = []
        collapse run = [Left (TSpace, snd . unwrapL . head $ run)]

elimSides :: ElimInterm -> ElimInterm
elimSides = elimLeft . elimRight
    where
        elimLeft = dropWhile isSpaceIntermItem
        elimRight = reverse . elimLeft . reverse

isSpaceIntermItem (Left (x, _)) = isSpace x
isSpaceIntermItem _             = False

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

spaces :: Parser [Token]
spaces = many $ token isSpace

attr :: Parser (String, [TokenExp])
attr = do
    token (== TBrace BDOpen)
    spaces

    attrName <- name "attribute name"
    spaces

    token (== TSplit) <?> "attribute split"
    val <- many exp
    token (== TBrace BDClose)
    spaces
    return (attrName, val)

sourcePos :: Parser SourcePos
sourcePos = liftM statePos getParserState

tag :: Parser TokenExp
tag = do
    token (== TBrace BDOpen)
    spaces

    nameSL <- sourceLoc
    tagName <- name "tag name"
    spaces

    attrsSL <- sourceLoc
    attrs <- many attr >>= return . (M.map ExpList) . M.fromList
    spaces

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
strChar = token isChar <|> token isSpace

str :: Parser TokenExp
str = do
    sl <- sourceLoc
    cs <- many1 strChar
    return $ Str cs (SMStr sl)

isChar (TChar _)                    = True
isChar _                            = False

-----------------------------------------
--- Parser Runner and Post-Processing ---
-----------------------------------------

-- Whitespace handling requires tokens in order to distinguish
-- between escaped and bare whitespace.

type TokenExp = ExpAux Token
type TokenAttrMap = AttrMapAux Token
type TokenExpList = ExpListAux Token

tokToChar :: Token -> Char
tokToChar (TChar c)         = c
tokToChar TSpace            = ' '

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
