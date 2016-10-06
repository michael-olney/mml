{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module MML.Funs (MacroFun, MacroFuns, funs) where

import MML
import MML.Types
import MML.Parse
import MML.Eval
import qualified Data.Map as M
import Data.Char
import Data.List
import System.IO
import Codec.Picture
import qualified Vision.Image as V
import Vision.Primitive.Shape
import Vision.Image.JuicyPixels
import Vision.Image.Transform
import System.IO
import System.Directory
import System.FilePath
import Control.Monad
import Control.Monad.Extra
import Codec.Archive.Zip
import Data.ByteString.Lazy as B (writeFile)

convParam (Tag name [] (Just v)) = (name, v)
convParam _ = error "wrong form for parameter"

inc :: [Cont] -> IO [Cont]
inc ((Str fn):rest) = do
    let params = M.fromList . (map convParam) $ rest
    ih <- openBinaryFile fn ReadMode
    inp <- hGetContents ih
    r <- parse params funs ("include: " ++ fn) inp
    doc <- (case r of
        (Left err)  -> error $ "parse error: " ++ (show err)
        (Right doc) -> return doc
        )
    doc2 <- eval params funs doc
    let (Doc conts) = doc2

    hClose ih
    return conts
inc _ = error "wrong form for macro 'inc'"

-- XXX split into three ops

subst :: Params -> [Cont] -> [Cont]
subst params = auxList
    where
        aux (Tag name attrs children)
            | M.member name params  = params M.! name
            | otherwise             =
                let
                    (keys, vals) = unzip attrs
                    attrs2 = zip keys (map auxList vals)
                in
                    [Tag name attrs2 ((liftM auxList) children)]
        aux e@(Str _) = [e]
        aux (Macro name cs) = [Macro name (auxList cs)]
        auxList = concatMap aux
subst _ = error "wrong form for macro 'include'"

convThumbChar '/' = '_'
convThumbChar x = x

resizeFriday w h img = resize Bilinear (ix2 h w) img

resizeJuicy (ImageRGB8 img) w h =
    ImageRGB8 . toJuicyRGB . (resizeFriday w h) . toFridayRGB $ img
resizeJuicy (ImageRGBA8 img) w h =
    ImageRGBA8 . toJuicyRGBA . (resizeFriday w h). toFridayRGBA $ img
resizeJuicy _ w h = error "unsupported pixel format"

resizesingle :: Cont -> IO Cont
resizesingle e@(Tag "img" as Nothing) = do
    let asm = M.fromList as
    let attr = unwrapStr . (asm M.!)
    let src = attr $ "src"
    let w::Int = read . attr $ "width"
    let h::Int = read . attr $ "height"
    let thumbdst = "gen/"++ (attr "width") ++ "x" ++ (attr "height") ++ (map convThumbChar src) 
    putStr ("resizing " ++ src ++ "...")
    (Right img) <- readImage src
    let resized = resizeJuicy img w h
    savePngImage thumbdst resized
    putStr "OK\n"

    let asm2 = (M.insert "src" [Str thumbdst]) . (M.delete "width") . (M.delete "height") $ asm

    return (Tag "img" (M.toList asm2) Nothing)
resizesingle e@(Tag "img" as (Just xs)) = do
    ys <- resizeimgs xs
    (Tag "img" as2 Nothing) <- resizesingle (Tag "img" as Nothing)
    return (Tag "img" as2 (Just ys))
resizesingle e@(Tag name as Nothing) = return e
resizesingle (Tag name as (Just x)) = do
    y <- resizeimgs x
    return (Tag name as (Just y))
resizesingle x = return x

resizeimgs :: [Cont] -> IO [Cont]
resizeimgs = mapM resizesingle

filesize :: [Cont] -> IO [Cont]
filesize [(Str fn)] = do
    h <- openFile fn ReadMode
    sz <- hFileSize h
    hClose h
    return . (:[]) . Str . show $ sz
filesize e = error ("bad usage for macro 'filesize'" ++ (show e))

linksingle e@(Tag "img" as (Just [Tag "img" as2 Nothing])) =
    let
        src = unwrapStr ((M.fromList as2) M.! "src")
    in
        Tag "a" [("href", [Str ("javascript:showImage('" ++ src ++ "');")])] (
            Just [Tag "img" as Nothing])
linksingle e@(Tag "img" as x) =
    linksingle (Tag "img" as (Just [Tag "img" as Nothing]))
linksingle (Tag name as (Just x)) = Tag name as (Just (linkimgs x))
linksingle e = e

linkimgs :: [Cont] -> [Cont]
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

prettyfilesize :: [Cont] -> [Cont]
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

listpresskitshots :: [Cont] -> IO [Cont]
listpresskitshots (Tag "path" [] (Just [Str fn]):(Tag "suffix" [] (Just [Str suffix])):[]) = do
    xs <- getDirectoryContents fn
    let aux x = take ((length x) - (length suffix)) x
    return . (map Str) . (map aux) . filter (isSuffixOf suffix) $ xs
listpresskitshots _ = error "bad usage of macro listpresskitshots"

substlist :: ([Cont] -> IO [Cont]) -> [Cont] -> IO [Cont]
substlist evalFun ((Tag "list" [] (Just xs)):(Tag "bind" [] (Just vnconts)):(Tag "targ" [] (Just targ)):[]) =
    let
        aux varname item = do
            let x = (subst (M.fromList [(varname, [item])]) targ)
            evalFun x
    in do
        varnameConts <- evalFun vnconts
        (case varnameConts of
            [Str varname] -> (evalFun xs) >>= concatMapM (aux varname)
            e -> error ("bad varname in substlist:" ++ (show e))
            )
substlist _ e = error ("bad usage of macro substlist:" ++ (show e))

dropPath :: Int -> FilePath -> FilePath
dropPath n = joinPath . (drop n) . splitPath

withEntryPath :: (FilePath -> FilePath) -> Entry -> Entry
withEntryPath f e = e {eRelativePath = f . eRelativePath $ e}

unwrapJust :: Maybe a -> a
unwrapJust Nothing = error "unwrap Just failed"
unwrapJust (Just x) = x

createzip :: [Cont] -> IO [Cont]
createzip ((Tag "outfilename" [] (Just [Str outfn])):(Tag "filenames" [] (Just fs)):(Tag "pathtrim" [] (Just [Str pathtrimstr])):(Tag "outbase" [] (Just [Str outbase])):[]) = do
    putStr ("Creating " ++ outfn ++ "..\n")
    let (pathtrim::Int) = read pathtrimstr
    let options = [OptRecursive, OptLocation "" True]
    archive <- addFilesToArchive options emptyArchive(map unwrap1Str fs) 
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

strictIO f g c = (g c) >>= f
strict f g c = (g c) >>= return . f

funs :: MacroFuns
funs = M.fromList [
    ("resizeimgs", strictIO resizeimgs),
    ("prettyfilesize", strict prettyfilesize),
    ("linkimgs", strict linkimgs),
    ("filesize", strictIO filesize),
    ("listpresskitshots", strictIO listpresskitshots),
    ("substlist", substlist),
    ("createzip", strictIO createzip),
    ("inc", strictIO inc)
    ]

