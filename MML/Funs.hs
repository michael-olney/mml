{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module MML.Funs (MacroFun, MacroFuns, funs) where

import MML
import MML.Types
import MML.Parse
import MML.Eval
import MML.Scripting (runScript)

import Prelude hiding (readFile)
import Data.Char
import Data.List
import Codec.Picture
import qualified Vision.Image as V
import Vision.Primitive.Shape
import Vision.Image.JuicyPixels
import Vision.Image.Transform
import System.IO
import qualified System.IO.Strict as SIO
import System.Directory
import System.FilePath
import System.Exit
import Control.Monad
import Control.Monad.Extra
import Codec.Archive.Zip
import Data.ByteString.Lazy as B (writeFile)
import qualified Data.Map as M
import Data.Map ((!))

convParam (Tag (Str name) (M.toList -> []) (Just v)) = (name, v)
convParam (Tag _ (M.toList -> []) _)        = error "shouldn't see this"
convParam _                                 = error "wrong form for parameter"

rawinc :: [Exp] -> IO [Exp]
rawinc ((Str fn):rest) = do
    let bindings = M.fromList . (map convParam) $ rest
    inp <- SIO.readFile fn
    return [Str inp]
raw inc _ = error "wrong form for macro 'rawinc'"

inc :: [Exp] -> IO [Exp]
inc ((Str fn):rest) = do
    let bindings = M.fromList . (map convParam) $ rest
    inp <- SIO.readFile fn
    r <- parse ("include: " ++ fn) inp
    doc <- (case r of
        (Left err)  -> error $ "parse error: " ++ err
        (Right doc) -> return doc
        )
    doc2 <- eval (Ctx [] bindings funs) doc

    return doc2
inc _ = error "wrong form for macro 'inc'"

-- XXX split into three ops

subst :: Ctx -> [Exp] -> [Exp]
subst ctx@(Ctx tb env funs) = auxList
    where
        aux (Var name)
            | M.member name env     = env ! name
            | otherwise             = error $ "unbound variable: " ++ name
        aux (Tag name attrs children) =
                let
                    {-(keys, vals) = unzip . M.toList $ attrs
                    attrs2 = zip keys (map auxList vals)-}
                    attrs2 = M.map auxList attrs
                    [name2] = aux name
                in
                    [Tag name2 attrs2 ((liftM auxList) children)]
        aux e@(Tag _ _ _ )          = [e]
        aux e@(Str _) = [e]
        aux (Call tb name cs) = [Call tb name (auxList cs)]
        auxList = concatMap aux

convThumbChar '/' = '_'
convThumbChar x = x

resizeFriday w h img = resize Bilinear (ix2 h w) img

resizeJuicy (ImageRGB8 img) w h =
    ImageRGB8 . toJuicyRGB . (resizeFriday w h) . toFridayRGB $ img
resizeJuicy (ImageRGBA8 img) w h =
    ImageRGBA8 . toJuicyRGBA . (resizeFriday w h). toFridayRGBA $ img
resizeJuicy _ w h = error "unsupported pixel format"

resizesingle :: Exp -> IO Exp
resizesingle e@(Tag (Str "img") as Nothing) = do
    let attr = unwrapStr . (as !) . Str
    let src = attr $ "src"
    let w::Int = read . attr $ "width"
    let h::Int = read . attr $ "height"
    let thumbdst = "gen/"++ (attr "width") ++ "x" ++ (attr "height") ++ (map convThumbChar src) 
    putStr ("resizing " ++ src ++ "...")
    (Right img) <- readImage src
    let resized = resizeJuicy img w h
    savePngImage thumbdst resized
    putStr "OK\n"

    -- TODO clean up
    let as2 = (M.insert (Str "src") [Str thumbdst]) . (M.delete (Str "width")) . (M.delete (Str "height")) $ as

    return (Tag (Str "img") as2 Nothing)
resizesingle e@(Tag (Str "img") as (Just xs)) = do
    ys <- resizeimgs xs
    (Tag (Str "img") as2 Nothing) <- resizesingle (Tag (Str "img") as Nothing)
    return (Tag (Str "img") as2 (Just ys))
resizesingle e@(Tag name as Nothing) = return e
resizesingle (Tag name as (Just x)) = do
    y <- resizeimgs x
    return (Tag name as (Just y))
resizesingle x = return x

resizeimgs :: [Exp] -> IO [Exp]
resizeimgs = mapM resizesingle

filesize :: [Exp] -> IO [Exp]
filesize [(Str fn)] = do
    h <- openFile fn ReadMode
    sz <- hFileSize h
    hClose h
    return . (:[]) . Str . show $ sz
filesize e = error ("bad usage for macro 'filesize'" ++ (show e))

linksingle e@(Tag (Str "img") as (Just [Tag (Str "img") as2 Nothing])) =
    let
        src = unwrapStr (as2 ! (Str "src"))
        attrs = [(Str "href", [Str ("javascript:showImage('" ++ src ++ "');")])]
    in
        Tag (Str "a") (M.fromList attrs) (
            Just [Tag (Str "img") as Nothing])
linksingle e@(Tag (Str "img") as x) =
    linksingle (Tag (Str "img") as (Just [Tag (Str "img") as Nothing]))
linksingle (Tag name as (Just x)) = Tag name as (Just (linkimgs x))
linksingle e = e

linkimgs :: [Exp] -> [Exp]
linkimgs = map linksingle

kilobyte = 1024 :: Int
megabyte = kilobyte*1024
gigabyte = megabyte*1024
terabyte = gigabyte*1024

units = [
    ("TB", terabyte),
    ("GB", gigabyte),
    ("MB", megabyte),
    ("KB", kilobyte)
    ]


prettyfilesize :: [Exp] -> [Exp]
prettyfilesize [(Str x)] =
    let
        v = read x
        aux [] = (show x) ++ " bytes"
        aux ((units, unitv):xs) = (case v >= unitv of
            True -> (show (v `div` unitv)) ++ units
            False -> aux xs)
    in
        [Str (aux units)] 
prettyfilesize _ = error "bad usage of macro prettyfilesize"

listpresskitshots :: [Exp] -> IO [Exp]
listpresskitshots (Tag (Str "path") (M.toList -> []) (Just [Str fn]):(Tag (Str "suffix") (M.toList -> []) (Just [Str suffix])):[]) = do
    xs <- getDirectoryContents fn
    let aux x = take ((length x) - (length suffix)) x
    return . (map Str) . (map aux) . filter (isSuffixOf suffix) $ xs
listpresskitshots _ = error "bad usage of macro listpresskitshots"

nestEnv :: Env -> Env -> Env
nestEnv outer inner = M.union inner outer

substlist :: Ctx -> (Ctx -> [Exp] -> IO [Exp]) -> [Exp] -> IO [Exp]
substlist ctx@(Ctx tb env funs) evalFun ((Tag (Str "list") (M.toList -> []) (Just xs)):(Tag (Str "bind") (M.toList -> []) (Just vnexps)):(Tag (Str "targ") (M.toList -> []) (Just targ)):[]) =
    let
        aux varname item = do
            let env' = nestEnv (M.fromList [(varname, [item])]) env
            let x = (subst (Ctx tb env' funs)targ)
            evalFun ctx x
    in do
        varnameExps <- evalFun ctx vnexps
        (case varnameExps of
            [Str varname] -> (evalFun ctx xs) >>= concatMapM (aux varname)
            e -> error ("bad varname in substlist:" ++ (show e))
            )
substlist _ _ e = error ("bad usage of macro substlist:" ++ (show e))

dropPath :: Int -> FilePath -> FilePath
dropPath n = joinPath . (drop n) . splitPath

withEntryPath :: (FilePath -> FilePath) -> Entry -> Entry
withEntryPath f e = e {eRelativePath = f . eRelativePath $ e}

unwrapJust :: Maybe a -> a
unwrapJust Nothing = error "unwrap Just failed"
unwrapJust (Just x) = x

createzip :: [Exp] -> IO [Exp]
createzip ((Tag (Str "outfilename") (M.toList -> []) (Just [Str outfn])):(Tag (Str "filenames") (M.toList -> []) (Just fs)):(Tag (Str "pathtrim") (M.toList -> []) (Just [Str pathtrimstr])):(Tag (Str "outbase") (M.toList -> []) (Just [Str outbase])):[]) = do
    putStr ("Creating " ++ outfn ++ "..\n")
    let (pathtrim::Int) = read pathtrimstr
    let options = [OptRecursive, OptLocation "" True]
    archive <- addFilesToArchive options emptyArchive (map unwrap1Str fs) 
    let fia = filesInArchive archive
    let es = map (unwrapJust . (\fn -> findEntryByPath fn archive)) fia
    let es2 = map (withEntryPath (dropPath pathtrim)) es
    let es3 = map (withEntryPath (outbase </>)) es2
    let archive2 = foldr addEntryToArchive emptyArchive es3
    putStr "archive contents:\n"
    let fia2 = filesInArchive archive2
    mapM_ (\x -> putStr (x ++ "\n")) fia2
    let bs = fromArchive archive2
    B.writeFile outfn bs
    return []
createzip e = error ("bad usage of macro createzip: " ++ (show e))

concatMacro :: [Exp] -> IO [Exp]
concatMacro xs  | allStrings    = return . (:[]) . Str . concat $ bareStrings
                | otherwise     = error $ "concat called on LIST with non-STRING element"
    where
        allStrings  = (foldr (&&) True) . (map isStr) $ xs
        bareStrings = map unwrap1Str xs

importscripts :: Ctx -> (Ctx -> [Exp] -> IO [Exp]) -> [Exp] -> IO [Exp]
importscripts ctx@(Ctx {ctxMacros = macros}) eval
    ((Tag (Str "scripts") _ (Just nameexps)):exps) = do
    nameexps2 <- eval ctx nameexps
    let names = map unwrap1Str nameexps2
    let addScript macros name =
            M.insert name (strictIO $ runScript name) macros
    let macros2 = foldl addScript macros names

    eval (ctx {ctxMacros = macros2}) exps
importscripts _ _ _ = error "bad usage of importscripts"

strictIO f ctx eval xs = (eval ctx xs) >>= f
strict f ctx eval xs = (eval ctx xs) >>= return . f

funs :: MacroFuns
funs = M.fromList [
    ("resizeimgs", strictIO resizeimgs),
    ("prettyfilesize", strict prettyfilesize),
    ("linkimgs", strict linkimgs),
    ("filesize", strictIO filesize),
    ("listpresskitshots", strictIO listpresskitshots),
    ("substlist", substlist),
    ("createzip", strictIO createzip),
    ("inc", strictIO inc),
    ("rawinc", strictIO rawinc),
    ("concat", strictIO concatMacro),
    ("importscripts", importscripts)
    ]

