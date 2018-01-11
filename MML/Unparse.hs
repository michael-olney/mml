module MML.Unparse (unparse) where

import MML.Types
import Text.PrettyPrint hiding (Doc, Str)
import qualified Text.PrettyPrint as PP
import Data.List (elem, groupBy, intersperse)
import qualified Data.Map as M
import Data.Map (Map)

unparse :: Doc -> String
unparse xs = fullRender LeftMode 1 1 aux2 "" (unparseExps . ExpList $ xs)
    where
        aux2 (PP.Chr c) x = c:x
        aux2 (PP.Str s) x = s ++ x

unparseExp :: Exp -> PP.Doc
unparseExp (Str xs _) = unparseStr xs
unparseExp (Tag name attrs children _) =
    text "<"
    <> unparseName name
    <> unparseAttrs attrs
    <> unparseChildren children
    <> text ">"

unparseExps :: ExpList -> PP.Doc
unparseExps = (foldr (<>) empty)
        . (map unparseExp)
        . unwrapExpList
    where
        groupFun x y                    = isStr x == isStr y
        isStr (Right (Str _ _))         = True
        isStr _                         = False

unparseChildren :: Maybe ExpList -> PP.Doc
unparseChildren Nothing     = empty
unparseChildren (Just xs)   = text "→" <> unparseExps xs

unparseAttrs :: Map String ExpList -> PP.Doc
unparseAttrs = (foldr (<>) empty) . (map unparseAttr) . M.toList

unparseAttr :: (String, ExpList) -> PP.Doc
unparseAttr (name, es) =
    text "<"
    <> unparseName name
    <> text "→"
    <> unparseExps es
    <> text ">"

unparseName :: String -> PP.Doc
unparseName = unparseStr

special = "<>{}→\\~^`"
whitespace = " \x0d\x0a\t"
escaped = special ++ whitespace

unparseChar :: Char -> [Char]
unparseChar x   | elem x escaped    = "\\" ++ [x]
                | otherwise         = [x]

unparseStr :: String -> PP.Doc
unparseStr "" = text "^"
unparseStr xs = text . (concatMap unparseChar) $ xs
