module MML.Scripting (runScript, beScript) where

import Prelude hiding (getContents, putStr)
import qualified Prelude as P (putStr)
import Data.ByteString.Lazy (pack, unpack, getContents, putStr)
import Data.ByteString.Internal (c2w, w2c)
import Data.Binary.Put (runPut)
import Data.Binary.Get (runGet)

import System.Process (readProcessWithExitCode)

import qualified Data.Map as M

import MML.Types
import MML.Binary

beScript :: ([Exp] -> IO [Exp]) -> IO ()
beScript f = do
    stdin <- getContents
    let params = (runGet getExps) $ stdin
    f params >>= putStr . runPut . putExps

runScript :: String -> [Exp] -> IO [Exp]
runScript path params = do
    let stdin = map w2c $ unpack $ runPut $ putExps params
    P.putStr (show $ runGet getExps $ runPut $ putExps params)
    (code, stdout, _) <- readProcessWithExitCode path [] stdin
    return . (runGet getExps) . pack . (map c2w) $ stdout
