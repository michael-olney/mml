module MML.Unparse (unparse) where

import MML.Types
import Text.PrettyPrint hiding (Doc, Str)
import qualified Text.PrettyPrint as PP
import Data.List (elem, groupBy, intersperse)
import qualified Data.Map as M
import Data.Map (Map)

data PseudoExp = StrSep

unparse :: Doc -> String
--unparse xs = unparseExp (Tag "?mml-version" [] [Str "0"])
unparse xs = fullRender LeftMode 1 1 aux2 "" (unparseExps xs)
    where
        aux2 (PP.Chr c) x = c:x
        aux2 (PP.Str s) x = s ++ x

unparseExp :: Exp -> PP.Doc
unparseExp (Var xs) =
    text "<$"
    <> unparseStr xs
    <> text ">"
unparseExp (Str xs) = unparseStr xs
unparseExp (Tag name attrs children) =
    text "<"
    <> unparseExp name
    <> unparseAttrs attrs
    <> unparseChildren children
    <> text ">"
unparseExp (Call _ name children) =
    text "<%"
    <> unparseExp name
    <> unparseChildren (Just children)
    <> text ">"

unparsePseudoExp :: Either PseudoExp Exp -> PP.Doc
unparsePseudoExp (Left StrSep)  = text "~"
unparsePseudoExp (Right e)      = unparseExp e

unparseExps :: [Exp] -> PP.Doc
unparseExps = (foldr (<>) empty)
        . (map unparsePseudoExp)
        . (concatMap separate)
        . (groupBy groupFun)
        . (map Right)
    where
        groupFun x y                    = isStr x == isStr y
        separate xs     | isStrList xs  = intersperse (Left StrSep) xs
                        | otherwise     = xs
        isStrList ((Right (Str _)):_)   = True
        isStrList (_:_)                 = False
        isStr (Right (Str _))           = True
        isStr _                         = False

unparseChildren :: Maybe [Exp] -> PP.Doc
unparseChildren Nothing     = empty
unparseChildren (Just xs)   = text ":" <> unparseExps xs

unparseAttrs :: Map Exp [Exp] -> PP.Doc
unparseAttrs = (foldr (<>) empty) . (map unparseAttr) . M.toList

unparseAttr :: (Exp, [Exp]) -> PP.Doc
unparseAttr (name, es) =
    text "<"
    <> unparseExp name
    <> text ":"
    <> unparseExps es
    <> text ">"

special = "<>{}:\\~%$^"
whitespace = " \x0d\x0a\t"
escaped = special ++ whitespace

unparseChar :: Char -> [Char]
unparseChar x   | elem x escaped    = "\\" ++ [x]
                | otherwise         = [x]

unparseStr :: String -> PP.Doc
unparseStr "" = text "^"
unparseStr xs = text . (concatMap unparseChar) $ xs
