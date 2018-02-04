{-# LANGUAGE OverloadedStrings #-}

module MML.Format.HTML (toHTML, fromHTML) where

import MML.Types

import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BS
import Prelude hiding (head, id, div, exp)
import Text.Blaze.Html5 hiding (title)
import Text.Blaze.Internal
import Text.Blaze.Renderer.Utf8 (renderMarkup)

toHTML :: Doc -> IO (Either String BS.ByteString)
toHTML doc = return . Right $ renderMarkup $ document doc

document doc = docTypeHtml $ exps doc

exps :: [Exp] -> Html
exps = mapM_ exp

expList :: ExpList -> Html
expList = exps . unwrapExpList

exp :: Exp -> Html
exp (Str str _) =
    toMarkup str
exp (Tag name attrs Nothing _) =
    applyAttrs attrs $ customLeaf (stringTag name) True
exp (Tag name attrs (Just body) _) =
    applyAttrs attrs $ customParent (stringTag name) (expList body)

applyAttrs :: AttrMap -> Html -> Html
applyAttrs attrs html = foldl func html $ M.toList attrs
    where
        func html (key, ExpList [Str str _]) =
            let
                tag = stringTag key
                val = stringValue str
                attribute = customAttribute tag val
            in
                html ! attribute
        func html (key, _)           = error "non-str encountered in attribute"

fromHTML :: String -> Doc
fromHTML = error "conversion from HTML not yet supported"

