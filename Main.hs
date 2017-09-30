module Main where

import MML
import MML.Funs
import MML.JSON
import System.IO
import System.Environment
import System.FilePath.Posix
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BS

data Format = HTML | JSON
    deriving (Eq, Ord, Show)

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
    r <- parse fn inp
    let doc = (case r of
            (Left err)  -> error $ "parse error: " ++ err
            (Right doc) -> doc
            )
    doc2 <- eval (Ctx [] M.empty funs) doc

    (hPutStrLn oh) . toHTML $ doc2

    hClose oh
    hClose ih

