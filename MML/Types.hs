{-# LANGUAGE DeriveDataTypeable #-}

module MML.Types where

import qualified Data.Map as M
import Data.Map (Map)
import Data.Data (Data)

type Doc = [Exp]

data SourceLoc = SourceLoc String Int Int
    deriving (Show, Eq, Ord, Data)
    
emptyTBR = SourceLoc "" 0 0

data Exp =
    Tag Exp (Map Exp [Exp]) (Maybe [Exp]) SourceLoc
    | Str String SourceLoc
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

