module MML.Format.JSON (toJSON, fromJSON) where

import MML.Types

import Data.Aeson.Types (Value(..))
import qualified Data.Aeson.Types as Aeson
import Data.Aeson (encode)

import qualified Data.Text as Txt
import qualified Data.Vector as Vec
import qualified Data.ByteString.Lazy as BS

import qualified Data.HashMap.Lazy as H
import qualified Data.Map as M
import qualified Data.List as L


expsToAeson :: [Exp] -> [Value]
expsToAeson = L.map expToAeson

attrToAeson :: (String, [Exp]) -> (Txt.Text, Value)
attrToAeson (key, exps) = (convKey key, convExps exps)
    where
        convKey = Txt.pack
        convExps = Aeson.Array . Vec.fromList . expsToAeson

attrsToAeson :: M.Map String [Exp] -> Value
attrsToAeson = Aeson.Object . H.fromList . (L.map attrToAeson) . M.toList

childrenToAeson Nothing = Null
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

fromJSON = error "conversion from JSON not yet implemented"
