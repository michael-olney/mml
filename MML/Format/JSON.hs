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


unwrap1StrAeson = Aeson.String . Txt.pack . unwrap1Str 

expsToAeson :: [Exp] -> [Value]
expsToAeson = L.map expToAeson

attrToAeson :: (Exp, [Exp]) -> (Txt.Text, Value)
attrToAeson (key, exps) = (convKey key, convExps exps)
    where
        convKey = Txt.pack . unwrap1Str
        convExps = Aeson.Array . Vec.fromList . expsToAeson

attrsToAeson :: M.Map Exp [Exp] -> Value
attrsToAeson = Aeson.Object . H.fromList . (L.map attrToAeson) . M.toList

childrenToAeson Nothing = Null
childrenToAeson (Just cs) = Aeson.Array . Vec.fromList . expsToAeson $ cs

sourceLocToAeson (SourceLoc loc lineno col) =
    Aeson.Object . H.fromList $ [
        (Txt.pack $ "loc", Aeson.String . Txt.pack $ loc),
        (Txt.pack $ "lineno", Aeson.Number . fromIntegral $ lineno),
        (Txt.pack $ "col", Aeson.Number . fromIntegral $ col)
        ]

expToAeson (Tag name attrs children srcloc) =
    Aeson.Object . H.fromList $ [
        (Txt.pack "name", unwrap1StrAeson name),
        (Txt.pack "attrs", attrsToAeson attrs),
        (Txt.pack "children", childrenToAeson children),
        (Txt.pack "sourceloc", sourceLocToAeson srcloc)
    ]
expToAeson (Str exp tb) =
    Aeson.String . Txt.pack $ exp
expToAeson _ = error "unsupported expression type"

toJSON :: Doc -> IO (Either String BS.ByteString)
toJSON = return . Right . encode . expsToAeson

fromJSON = error "conversion from JSON not yet implemented"
