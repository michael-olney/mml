{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module MML.Format.HTML (toHTML, fromHTML) where

import MML.Types

import Control.Monad
import Control.Monad.Trans.Class
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BS
import Prelude hiding (head, id, div, exp)
import Text.BlazeT.Html5 hiding (title)
import Text.BlazeT.Internal
import Text.BlazeT.Renderer.Utf8 (renderMarkup)

type MarkupE = MarkupT (Either String) ()

toHTML :: Doc -> IO (Either String BS.ByteString)
toHTML doc = return $ execWith renderMarkup $ document doc

document :: Doc -> MarkupE
document doc = docTypeHtml $ exps doc

exps :: [Exp] -> MarkupE
exps = mapM_ exp

expList :: ExpList -> MarkupE
expList = exps . unwrapExpList

exp :: Exp -> MarkupE
exp (Str str _) =
    toMarkup str
exp (Tag name attrs Nothing _) =
    applyAttrs attrs $ customLeaf (stringTag name) True
exp (Tag name attrs (Just body) _) =
    applyAttrs attrs $ customParent (stringTag name) (expList body)

applyAttrs :: AttrMap -> MarkupE -> MarkupE
applyAttrs attrs html =
    case foldM func html $ M.toList attrs of
        Left err    -> lift $ Left err
        Right act   -> act
    where
        func html (key, ExpList [Str str _]) =
            let
                tag = stringTag key
                val = stringValue str
                attribute = customAttribute tag val
            in
                Right $ html ! attribute
        func html (key, _) = Left "non-str encountered in attribute"

fromHTML :: String -> Doc
fromHTML = error "conversion from HTML not yet supported"

