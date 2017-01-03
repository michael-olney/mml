module MML.Scripting (runScript, beScript) where

import Prelude hiding (getContents, putStr, writeFile)
import qualified Prelude as P (putStr)
import qualified System.IO as SIO (writeFile)
import Data.ByteString.Lazy.Char8 (pack, unpack, getContents, putStr, writeFile)
import qualified Data.ByteString.Lazy.Char8 as B (length)
import Data.Binary.Put (runPut)
import Data.Binary.Get (runGet)
import Control.Exception (catch, SomeException, displayException)

import System.Process.ByteString.Lazy (readProcessWithExitCode)

import qualified Data.Map as M

import MML.Types
import MML.Binary

beScript :: ([Exp] -> IO [Exp]) -> IO ()
beScript f = do
    stdin <- getContents
    let params = (runGet getExps) $ stdin
    f params >>= putStr . runPut . putExps

runScript :: String -> [Exp] -> IO [Exp]
runScript name params = do
    let path = "mml-" ++ name
    let stdin = runPut $ putExps params
    (code, stdout, stderr) <- readProcessWithExitCode path [] stdin
    return . (runGet getExps) $ stdout
