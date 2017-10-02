module MML.Format.MMLPure (toMMLPure, fromMMLPure) where

import MML.Parse

import Data.ByteString.Lazy.UTF8 (toString)

toMMLPure = error "conversion to MML-Pure not yet supported"

fromMMLPure infile bs = do
    let str = toString bs
    parse infile str

