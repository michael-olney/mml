module MML.Format.MML (toMML, fromMML) where

import MML.Parse

import Data.ByteString.Lazy.UTF8 (toString)

toMML = error "conversion to MML not yet supported"

fromMML infile bs = do
    let str = toString bs
    parse infile str

