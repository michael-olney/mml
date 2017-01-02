module Main where

import MML.Types
import MML.Scripting

script :: [Exp] -> IO [Exp]
script params = return [Str "Hello, world!"]

main :: IO ()
main = beScript script