module Main where

import MML
import MML.Funs
import System.IO
import System.Environment
import System.FilePath.Posix
import qualified Data.Map as M

must :: Bool -> String -> IO ()
must False xs   = error xs
must True _     = return ()

main = do
    args <- getArgs
    must (length args == 1) "PROG file.mml"
    let fn = args !! 0
    must (takeExtension fn == ".mml") "extension must be .mml"

    let outFile = replaceExtension fn ".html"

    ih <- openBinaryFile fn ReadMode
    oh <- openBinaryFile outFile WriteMode

    inp <- hGetContents ih
    doc <- parseMML M.empty funs fn inp
    (hPutStr oh) . toHTML $ doc
    hPutStr oh "\n"

    hClose oh
    hClose ih

    putStr . show $ doc
    putStr "\n"
