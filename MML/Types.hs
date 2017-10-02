{-# LANGUAGE DeriveDataTypeable #-}

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

data Exp =
    Tag String (M.Map String [Exp]) (Maybe [Exp]) SourceMap
    | Str String SourceMap
    deriving (Show, Eq, Ord, Data)

isStr :: Exp -> Bool
isStr (Str _ _) = True
isStr _         = False

unwrap1Str :: Exp -> String
unwrap1Str (Str x _) = x
unwrap1Str _ = error "tried to unwrap non-str"

unwrapStr :: [Exp] -> String
unwrapStr [Str x _] = x
unwrapStr (x:xs) = error "tried to unwrap non-singular str"
unwrapStr _ = error "tried to unwrap non-str"

