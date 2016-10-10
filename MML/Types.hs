module MML.Types where

import qualified Data.Map as M

type Doc = [Exp]

data TracebackRecord = TracebackRecord String Int Int String
    deriving (Show, Eq)

type Traceback = [TracebackRecord]

data Exp =
    Tag String [(String, [Exp])] (Maybe [Exp])
    | Str String
    | Call TracebackRecord String [Exp]
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

type MacroFunAux a = a -> (a -> [Exp] -> IO [Exp]) -> [Exp] -> IO [Exp]
type MacroFunsAux a = M.Map String (MacroFunAux a)

type Params = M.Map String [Exp]

data Ctx = Ctx Traceback Params (MacroFunsAux Ctx)
type MacroFun = MacroFunAux Ctx
type MacroFuns = MacroFunsAux Ctx
