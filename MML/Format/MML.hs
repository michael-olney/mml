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
import Control.DeepSeq

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
                Right mml   -> subst env $ mml

    hClose ih

    inlineExps mml

tryReadFile :: String -> IO [Exp]
tryReadFile path = do
    ih <- openBinaryFile path ReadMode
    bs <- BS.hGetContents ih

    let sm = SMStr $ SourceLoc path 0 0
    let text = toString bs

    deepseq bs (hClose ih)

    return [Str text sm]

inlineExps :: [Exp] -> IO [Exp]
inlineExps = everywhereM $ mkM inline
    where
        inline :: [Exp] -> IO [Exp]
        inline (Tag ('@':src) env Nothing sm:xs) = do
            let uenv = M.map unwrapExpList env
            head <- (tryInclude uenv src) >>= inline
            return $ head ++ xs
        inline (Tag "#readfile" env (Just (ExpList path)) sm:xs) = do
            head <- tryReadFile $ unwrapStr path
            return $ head ++ xs
        inline (Tag "#readfile" env Nothing sm:xs) =
            error "missing file path in #readfile"
        inline x =
            return x

fromMML :: String -> BS.ByteString -> IO (Either String Doc)
fromMML infile bs = do
    mmlp <- Pure.fromMMLPure infile bs
    case mmlp of
        Left err    -> return . Left $ err
        Right mmlp  -> do
            mml <- inlineExps $ mmlp
            return . Right $ mml

