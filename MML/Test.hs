{-# LANGUAGE RankNTypes #-}

module Main where

import MML
import MML.Lex
import qualified Data.Map as M

import Data.Data
import Data.Generics.Aliases
import Data.Generics.Schemes

import Test.HUnit
import System.Exit

relaxedEq :: (Eq a, Data a) => a -> a -> Bool
relaxedEq x y = (norm x) == (norm y)
    where
        norm x = everywhere (mkT $ \_ -> dummySM) x

assertEqualRelaxed pref exp got = do
    let msg = pref ++ "Expected:\n" ++ (show exp)
                ++ "\n. Got:\n" ++ (show got) ++ "\n"
    assertBool msg (relaxedEq exp got)

parseEqTest str v = TestCase (do
        r <- parse "<unknown>" str
        (case r of 
            (Left e)    -> assertBool ("parser should accept this: " ++ e) False
            (Right doc) -> assertEqualRelaxed "" v doc
            )        
        )
parseRejectTest str = TestCase (do
        r <- parse "<unknown>" str
        (case r of
            (Right _)   -> assertBool "parser should reject this" False
            _           -> return ()
            )
        )

roundTripTestFun e = TestCase (do
        let txt = unparse e
        r <- parse "<unknown>" txt
        (case r of
            (Right e2)   -> do
                let msg = "round trip failed. expected:\n"
                        ++ (show e) ++ "\ngot:\n" ++ (show e2)
                assertEqualRelaxed "round trip failed: " e e2
            (Left err)   -> (assertBool
                                ("roundtrip rejection ("
                                ++ err
                                ++ "): " ++ txt)
                                False
                                )
            )
        )

tests = TestList [
    ]

main :: IO ()
main = do
    r <- runTestTT $ tests
    if failures r > 0 then System.Exit.exitFailure else return ()
