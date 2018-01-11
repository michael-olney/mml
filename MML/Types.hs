{-# LANGUAGE DeriveDataTypeable, DeriveFunctor #-}

module MML.Types where

import qualified Data.Map as M
import Data.Data (Data)

type Doc = [Exp]

data SourceLoc = SourceLoc {
        slPath :: String,
        slLineIndex :: Int,
        slColIndex :: Int
    }
    deriving (Show, Eq, Ord, Data)

data SourceMap =
    SMTag {
        smTagName :: SourceLoc,
        smTagAttrs :: SourceLoc,
        smTagChildren :: SourceLoc
    }
    | SMStr {
        smStrLoc :: SourceLoc
    }
    deriving (Show, Eq, Ord, Data)

dummySL = SourceLoc "" 0 0
dummySM = SMStr { smStrLoc = dummySL }

data ExpAux a =
    Tag String (AttrMapAux a) (Maybe (ExpListAux a)) SourceMap
    | Str [a] SourceMap
    deriving (Show, Eq, Ord, Data, Functor)

data ExpListAux a = ExpList [ExpAux a]
    deriving (Show, Eq, Ord, Data, Functor)

type AttrMapAux a = M.Map String (ExpListAux a)

type Exp = ExpAux Char
type AttrMap = AttrMapAux Char
type ExpList = ExpListAux Char

unwrapExpList (ExpList xs) = xs

unwrapStr :: [Exp] -> String
unwrapStr [Str x _] = x
unwrapStr (x:xs) = error "tried to unwrap non-singular str"
unwrapStr [] = error "tried to unwrap [] as str"

