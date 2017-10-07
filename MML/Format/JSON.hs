{-# LANGUAGE ViewPatterns #-}

module MML.Format.JSON (toJSON, fromJSON) where

import MML.Types

import Data.Aeson.Types (Value(..))
import qualified Data.Aeson.Types as Aeson
import Data.Aeson (encode, decode)

import qualified Data.Text as Txt
import qualified Data.Vector as Vec
import qualified Data.ByteString.Lazy as BS

import qualified Data.HashMap.Lazy as H
import qualified Data.Map as M
import qualified Data.List as L

-- --------------------------
-- Encoding
-- --------------------------

expsToAeson :: [Exp] -> [Value]
expsToAeson = L.map expToAeson

attrToAeson :: (String, [Exp]) -> (Txt.Text, Value)
attrToAeson (key, exps) = (convKey key, convExps exps)
    where
        convKey = Txt.pack
        convExps = Aeson.Array . Vec.fromList . expsToAeson

attrsToAeson :: M.Map String [Exp] -> Value
attrsToAeson = Aeson.Object . H.fromList . (L.map attrToAeson) . M.toList

childrenToAeson Nothing = Aeson.Null
childrenToAeson (Just cs) = Aeson.Array . Vec.fromList . expsToAeson $ cs

sourceLocToAeson (SourceLoc loc lineno col) =
    Aeson.Object . H.fromList $ [
        (Txt.pack $ "loc", Aeson.String . Txt.pack $ loc),
        (Txt.pack $ "lineno", Aeson.Number . fromIntegral $ lineno),
        (Txt.pack $ "col", Aeson.Number . fromIntegral $ col)
        ]

sourceMapToAeson (SMStr {smStrLoc=srcloc}) =
    Aeson.Array . Vec.fromList $ [
        Aeson.String . Txt.pack $ "str_source_map",
        Aeson.Object . H.fromList $ [
            (Txt.pack $ "srcloc", sourceLocToAeson srcloc)
        ]
    ]
sourceMapToAeson (SMTag {
    smTagName=smName,
    smTagAttrs=smAttrs,
    smTagChildren=smChildren
    }) = Aeson.Array . Vec.fromList $ [
        Aeson.String . Txt.pack $ "tag_source_map",
        Aeson.Object . H.fromList $ [
            (Txt.pack $ "tag_name_sm", sourceLocToAeson smName),
            (Txt.pack $ "tag_attrs_sm", sourceLocToAeson smAttrs),
            (Txt.pack $ "tag_children_sm", sourceLocToAeson smChildren)
        ]
    ]

expToAeson (Tag name attrs children sm) =
    Aeson.Object . H.fromList $ [
        (Txt.pack "type", Aeson.String . Txt.pack $ "tag"),
        (Txt.pack "name", Aeson.String . Txt.pack $ name),
        (Txt.pack "attrs", attrsToAeson attrs),
        (Txt.pack "children", childrenToAeson children),
        (Txt.pack "source_map", sourceMapToAeson sm)
    ]
expToAeson (Str val sm) =
    Aeson.Object . H.fromList $ [
        (Txt.pack "type", Aeson.String . Txt.pack $ "str"),
        (Txt.pack "value", Aeson.String . Txt.pack $ val),
        (Txt.pack "source_map", sourceMapToAeson sm)
    ]

toJSON :: Doc -> IO (Either String BS.ByteString)
toJSON = return . Right . encode . expsToAeson

-- --------------------------
-- Decoding
-- --------------------------

fromJSON :: String -> BS.ByteString -> IO (Either String Doc)
fromJSON infile bs = do
    case decode bs of
        Nothing     -> return . Left $ "JSON failed to parse"
        Just json   -> return $ expsFromJSON json

packlup = H.lookup . Txt.pack

getAttr :: String -> Aeson.Object -> Either String Aeson.Value
getAttr key (packlup key -> Just pval)  = Right pval
getAttr _ _                             = Left "missing entry"

exStr :: Aeson.Value -> Either String String
exStr (Aeson.String pval)   = Right . Txt.unpack $ pval
exStr _                     = Left "expected string value"

exSourceMap :: Aeson.Value -> Either String SourceMap
exSourceMap (Aeson.Array pval)  = Right dummySM -- XXX
exSourceMap _                   = Left "expected source map"

getStrAttr :: String -> Aeson.Object -> Either String String
getStrAttr key json = getAttr key json >>= exStr

exAttr :: (Txt.Text, Aeson.Value) -> Either String (String, [Exp])
exAttr (pkey, pval) = do
    val <- expsFromJSON pval
    Right (Txt.unpack pkey, val)

exAttrs :: Aeson.Value -> Either String (M.Map String [Exp])
exAttrs (Aeson.Object attrMap) =
    do
        attrList <- mapM exAttr $ H.toList attrMap
        return . M.fromList $ attrList
exAttrs _ = Left "expected object"

exChildren :: Aeson.Value -> Either String (Maybe [Exp])
exChildren Aeson.Null               = Right Nothing
exChildren json@(Aeson.Array _ )    = expsFromJSON json >>= return . Just

expFromJSON :: Aeson.Value -> Either String Exp
expFromJSON (Aeson.Object exp@(getStrAttr "type" -> Right "tag")) =
    do
        name <- getStrAttr "name" exp
        attrs <- getAttr "attrs" exp >>= exAttrs
        children <- getAttr "children" exp >>= exChildren
        sm  <- getAttr "source_map" exp >>= exSourceMap
        return $ Tag name attrs children sm
expFromJSON (Aeson.Object exp@(getStrAttr "type" -> Right "str")) =
    do
        val <- getStrAttr "value" exp
        sm  <- getAttr "source_map" exp >>= exSourceMap
        return $ Str val sm
expFromJSON _ =
    Left $ "expected expression while unpacking JSON"

expsFromJSON :: Aeson.Value -> Either String [Exp]
expsFromJSON (Aeson.Array exps) = (mapM expFromJSON) . Vec.toList $ exps
expsFromJSON _                  = Left $ "expected array while unpacking JSON"

