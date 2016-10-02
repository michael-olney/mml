module MML.Types where

import qualified Data.Map as M

{-
TODO:
1) Rename Cont to Value
2) Remove Doc constructor - make Doc a type synonym
-}

data Doc = Doc [Cont]
    deriving (Show, Eq)

data Cont =
    Tag String [(String, [Cont])] (Maybe [Cont])
    | Str String
    | Macro String [Cont]
    deriving (Show, Eq)

isStr :: Cont -> Bool
isStr (Str _) = True
isStr _ = False

unwrap1Str :: Cont -> String
unwrap1Str (Str x) = x
unwrap1Str _ = error "tried to unwrap non-str"

unwrapStr :: [Cont] -> String
unwrapStr [Str x] = x
unwrapStr (x:xs) = error "tried to unwrap non-singular str"
unwrapStr _ = error "tried to unwrap non-str"

type MacroFun = ([Cont] -> IO [Cont]) -> [Cont] -> IO [Cont]
type MacroFuns = M.Map String MacroFun

type Params = M.Map String [Cont]

