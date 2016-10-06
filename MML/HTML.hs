module MML.HTML (toHTML) where

import MML.Types
import Data.Char
import Data.List

header = "<!DOCTYPE HTML>"
footer = ""

toHTML :: Doc -> String
toHTML cs = header ++ (convExps cs) ++ footer

-- Intended to be just enough to cover both
-- character data and double-quoted attribute values
escape :: String -> String
escape s = aux s
    where
        aux []          = ""
        aux ('&':cs)    = "&amp;" ++ (aux cs)
        aux ('"':cs)    = "&quot;" ++ (aux cs)
        aux ('<':cs)    = "&lt;" ++ (aux cs)
        aux ('>':cs)    = "&gt;" ++ (aux cs)
        aux (c:cs)      = c:(aux cs)

-- XXX Doesn't cover the full range
isValidName :: String -> Bool
isValidName [] = False
isValidName (c:cs) = (start c) && (aux cs)
    where
        start c = (isAlpha c) || (c == ':') || (c == '_')
        aux [] = True
        aux (c:cs) = (val c) && (aux cs)
        val c = ((start c) || (isNumber c) || (c == '-')
            || (c == '.'))

convExps :: [Exp] -> String
convExps = foldl (\s c -> s ++ (conv c)) ""

convAttr :: (String, [Exp]) -> String
convAttr (name, [Str val]) = name ++ "=\"" ++ (escape val) ++ "\""
convAttr (name, []) = error "attribute values must not be empty"
convAttr (name, xs@(_:_)) = error ("attribute values must resolve to single value: " ++ (show xs))
convAttr _ = error "attribute values must resolve to strings"

convAttrs :: [(String, [Exp])] -> String
convAttrs = (intercalate " ") . (map convAttr)

convTag :: Exp -> String
convTag (Tag name [] Nothing) =
    "<" ++ name ++ "/>"
convTag (Tag name as Nothing) =
    "<" ++ name ++ " " ++ (convAttrs as) ++ "/>"
convTag (Tag name [] (Just cs)) =
    "<" ++ name ++ ">"
    ++ (convExps cs)
    ++ "</" ++ name ++ ">"
convTag (Tag name as (Just cs)) =
    "<" ++ name ++ " " ++ (convAttrs as) ++ ">"
    ++ (convExps cs)
    ++ "</" ++ name ++ ">"

conv :: Exp -> String
conv (Call _ _) = error "macro call encountered in MML.HTML"
conv t@(Tag name _ _)   | isValidName name  = convTag t
                        | otherwise         =
                            error ("bad tag name: {" ++ name ++ "}")
conv (Str s) = escape s

