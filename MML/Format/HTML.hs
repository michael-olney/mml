module MML.Format.HTML (toHTML, fromHTML) where

import MML.Types
import Data.Char
import Data.List
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.UTF8 as UTF8 (fromString)

header = "<!DOCTYPE HTML>"
footer = ""

toHTML :: Doc -> IO (Either String BS.ByteString)
toHTML doc = do
    let str = header ++ (convExps doc) ++ footer
    return . Right . UTF8.fromString $ str

fromHTML :: String -> Doc
fromHTML = error "conversion from HTML not yet supported"

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

-- TODO escape names..
convAttrAux :: String -> [Exp] -> String
convAttrAux name [Str val _] = name ++ "=\"" ++ (escape val) ++ "\""
convAttrAux name [] = error "attribute values must not be empty"
convAttrAux name xs@(_:_) = error ("attribute values must resolve to single value: " ++ (show xs))

convAttr :: (String, [Exp]) -> String
convAttr (name, xs) = convAttrAux name xs

convAttrs :: [(String, [Exp])] -> String
convAttrs = (intercalate " ") . (map convAttr)

convTag :: String -> [(String, [Exp])] -> Maybe [Exp] -> String
convTag name [] Nothing = 
    "<" ++ name ++ "/>"
convTag name as Nothing = 
    "<" ++ name ++ " " ++ (convAttrs as) ++ "/>"
convTag name [] (Just cs) = 
    "<" ++ name ++ ">"
    ++ (convExps cs)
    ++ "</" ++ name ++ ">"
convTag name as (Just cs) = 
    "<" ++ name ++ " " ++ (convAttrs as) ++ ">"
    ++ (convExps cs)
    ++ "</" ++ name ++ ">"

conv :: Exp -> String
conv t@(Tag name as children _)
                        | isValidName name  = convTag name (M.toList as) children
                        | otherwise         = error $ "bad tag name: " ++ name
conv (Str s _) = escape s

