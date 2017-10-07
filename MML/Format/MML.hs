{-# LANGUAGE ViewPatterns #-}

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

subst :: M.Map String [Exp] -> [Exp] -> [Exp]
subst env = everywhere $ mkT aux
    where
        aux :: [Exp] -> [Exp]
        aux (Tag ('$':name) (M.toList -> []) Nothing sm:xs)
            | M.member name env     = (env M.! name) ++ (aux xs)
            | otherwise             = error $ "unbound variable '" ++ name ++ "'"
        aux (Tag ('$':_) _ _ _:xs) =
            error "malformed variable tag"
        aux x =
            x

tryInclude :: M.Map String [Exp] -> String -> IO [Exp]
tryInclude env path = do
    ih <- openBinaryFile path ReadMode
    bs <- BS.hGetContents ih

    mmlp <- Pure.fromMMLPure path bs
    let mml = case mmlp of
                Left err    -> error err
                Right mml   -> subst env mml

    hClose ih

    inlineExps mml

inlineExps :: [Exp] -> IO [Exp]
inlineExps = everywhereM $ mkM inline
    where
        inline :: [Exp] -> IO [Exp]
        inline (Tag "#include" env (Just src) sm:xs) = do
            head <- (tryInclude env $ unwrapStr src) >>= inline
            tail <- inline xs
            return $ head ++ tail
        inline (Tag "#include" env Nothing sm:xs) =
            error "missing source path in #include"
        inline x =
            return x

fromMML :: String -> BS.ByteString -> IO (Either String Doc)
fromMML infile bs = do
    mmlp <- Pure.fromMMLPure infile bs
    case mmlp of
        Left err    -> return . Left $ err
        Right mmlp  -> do
            mml <- inlineExps mmlp
            return . Right $ mml

