module MML.Types where

import qualified Data.Map as M
import Data.Map (Map)

type Doc = [Exp]

data TracebackRecord = TracebackRecord String Int Int String
    deriving (Show, Eq, Ord)

emptyTBR = TracebackRecord "" 0 0 ""

type Traceback = [TracebackRecord]

data Exp =
    Tag Exp (Map Exp [Exp]) (Maybe [Exp])
    | Str String
    | Call TracebackRecord Exp [Exp]
    | Var String
    deriving (Show, Eq, Ord)

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

type Env = M.Map String [Exp]

data Ctx = Ctx {
    ctxTBR :: Traceback,
    ctxEnv :: Env,
    ctxMacros :: (MacroFunsAux Ctx)
}

type MacroFun = MacroFunAux Ctx
type MacroFuns = MacroFunsAux Ctx

emptyCtx = Ctx [] M.empty M.empty

makeList :: [Exp] -> Exp
makeList xs = Tag (Str "") M.empty (Just xs)

