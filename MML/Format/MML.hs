module MML.Format.MML (toMML, fromMML) where

import MML.Types
import MML.Parse
import qualified MML.Format.MMLPure as Pure

import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy.UTF8 (toString)
import System.IO

import Data.Generics.Schemes
import Data.Generics.Aliases

import Control.Monad.Extra

toMML = error "conversion to MML not yet supported"

tryInclude :: String -> IO [Exp]
tryInclude path = do
    ih <- openBinaryFile path ReadMode
    inp <- BS.hGetContents ih

    res <- fromMML path inp
    let mml = case res of
                Left err    -> error err
                Right mml   -> mml

    hClose ih

    return mml

inlineExps :: [Exp] -> IO [Exp]
inlineExps = everywhereM $ mkM inline
    where
        inline :: [Exp] -> IO [Exp]
        inline (Tag "#include" attrs children sm:xs)
            | M.member "src" attrs  = do
                head <- (tryInclude $ unwrapStr $ attrs M.! "src") >>= inline
                tail <- inline xs
                return $ head ++ tail
            | otherwise             = error "missing 'src' attribute in #include"
        inline x
            = return x

fromMML :: String -> BS.ByteString -> IO (Either String Doc)
fromMML infile bs = do
    mmlp <- Pure.fromMMLPure infile bs
    case mmlp of
        Left err    -> return . Left $ err
        Right mmlp  -> do
            mml <- inlineExps mmlp
            return . Right $ mml

