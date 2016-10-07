module MML.Unparse (unparse) where

import MML.Types
import Text.PrettyPrint hiding (Doc, Str)
import qualified Text.PrettyPrint as PP
import qualified Data.List (elem)

unparse :: Doc -> String
--unparse xs = aux (Tag "?mml-version" [] [Str "0"])
unparse xs = fullRender LeftMode 1 1 aux2 "" (aux xs)
    where
        aux = (foldr (<>) empty) . (map unparseExp)
        aux2 (PP.Chr c) x = c:x
        aux2 (PP.Str s) x = s ++ x

unparseExp :: Exp -> PP.Doc
unparseExp (Str xs) = unparseStr xs
unparseExp (Tag name attrs children) =
    text "<"
    <> unparseStr name
    <> unparseAttrs attrs
    <> unparseChildren children
    <> text ">"
unparseExp (Call name children) =
    text "{"
    <> unparseStr name
    <> unparseChildren (Just children)
    <> text "}"

unparseChildren :: Maybe [Exp] -> PP.Doc
unparseChildren Nothing     = empty
unparseChildren (Just xs)   = text ":" <> ((foldr (<>) empty) . (map unparseExp) $ xs)

unparseAttrs :: [(String, [Exp])] -> PP.Doc
unparseAttrs = (foldr (<>) empty) . (map unparseAttr)

unparseAttr :: (String, [Exp]) -> PP.Doc
unparseAttr (name, es) =
    text "<"
    <> unparseStr name
    <> text ":"
    <> ((foldr (<>) empty) . (map unparseExp)) es
    <> text ">"

special = "<>{}:\\"
whitespace = " \x0d\x0a\t"
escaped = special ++ whitespace

unparseChar :: Char -> [Char]
unparseChar x   | elem x escaped    = "\\" ++ [x]
                | otherwise         = [x]

unparseStr :: String -> PP.Doc
unparseStr = text . (concatMap unparseChar)
