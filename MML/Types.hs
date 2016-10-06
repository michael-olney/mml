module MML.Types where

import qualified Data.Map as M

type Doc = [Exp]

data Exp =
    Tag String [(String, [Exp])] (Maybe [Exp])
    | Str String
    | Call String [Exp]
    deriving (Show, Eq)

isStr :: Exp -> Bool
isStr (Str _) = True
isStr _ = False

unwrap1Str :: Exp -> String
unwrap1Str (Str x) = x
unwrap1Str _ = error "tried to unwrap non-str"

unwrapStr :: [Exp] -> String
unwrapStr [Str x] = x
unwrapStr (x:xs) = error "tried to unwrap non-singular str"
unwrapStr _ = error "tried to unwrap non-str"

type MacroFun = ([Exp] -> IO [Exp]) -> [Exp] -> IO [Exp]
type MacroFuns = M.Map String MacroFun

type Params = M.Map String [Exp]

