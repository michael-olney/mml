module MML.JSON (toJSON, fromJSON) where

import MML.Types

import Data.Aeson.Types (Value(..))
import qualified Data.Aeson.Types as Aeson
import Data.Aeson.Encode

import qualified Data.Text as Txt
import qualified Data.Vector as Vec

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

expToAeson (Tag name attrs children) =
    Aeson.Object . H.fromList $ [
        (Txt.pack "name", unwrap1StrAeson name),
        (Txt.pack "attrs", attrsToAeson attrs),
        (Txt.pack "children", childrenToAeson children)
    ]
expToAeson (Str exp) =
    Aeson.String . Txt.pack $ exp
expToAeson _ = error "unsupported expression type"

toJSON = encode . expsToAeson

fromJSON = error "unimplemented"
