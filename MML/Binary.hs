{-# LANGUAGE ScopedTypeVariables #-}

module MML.Binary (putExps, putExp, getExps, getExp) where

import Data.Binary (put, get, Binary, Get, Put)
import MML.Types
import Data.Word
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.Map as M

import Debug.Trace

putExp :: Exp -> Put
putExp = put

getExp :: Get Exp
getExp = get

putUTF8 xs = do
    put ((fromIntegral . length $ xs)::Word32)
    put . UTF8.fromString $ xs

putList f xs = do
    put ((fromIntegral . length $ xs)::Word32)
    mapM_ f xs

putAttr (name, exps) = do
    put name
    putExps exps

putAttrMap xs = putList putAttr $ M.toList xs

putExps :: [Exp] -> Put
putExps xs = putList put xs

getList :: Binary a => Get a -> Get [a]
getList f = do
    (count::Word32) <- get
    mapM (\x -> f) [1..count]

getExps :: Get [Exp]
getExps = getList get

getAttr = do
    (name::Exp) <- get
    (es::[Exp]) <- getExps
    return (name, es)

getAttrMap = getList getAttr >>= return . M.fromList

getUTF8 = do
    (count::Word32) <- get
    (bs::ByteString) <- get
    return . UTF8.toString $ bs

getExpWithType 0 = do
    name <- get
    attrmap <- getAttrMap
    es <- getExps
    return $ Tag name attrmap (Just es)
getExpWithType 1 = do
    name <- get
    attrmap <- getAttrMap
    return $ Tag name attrmap Nothing
getExpWithType 2 = getUTF8 >>= return . Str
getExpWithType 3 = do
    name <- get
    es <- getExps
    return $ Call (TracebackRecord "" 0 0 "") name es
getExpWithType 4 = getUTF8 >>= return . Var
getExpWithType x = fail $ "getExpWithType: " ++ (show x)

instance Binary Exp where
    put (Tag name attrmap (Just es)) = do
        put (0::Word32)
        put name
        putAttrMap attrmap
        putExps es
    put (Tag name attrmap Nothing) = do
        put (1::Word32)
        put name
        putAttrMap attrmap
    put (Str xs) = do
        put (2::Word32)
        putUTF8 xs
    put (Call tbr name es) = do
        put (3::Word32)
        put name
        putExps es
    put (Var name) = do
        put (4::Word32)
        putUTF8 name

    get = do
        (t::Word32) <- get
        getExpWithType t
