module Main where

import MML
import qualified Data.Map as M

import Test.HUnit
import System.Exit

parseEqTest str v = TestCase (do
        r <- parse M.empty M.empty "<unknown>" str
        (case r of 
            (Left _)    -> assertBool "parser should accept this" False
            (Right doc) -> assertEqual "" v doc
            )        
        )
parseRejectTest str = TestCase (do
        r <- parse M.empty M.empty "<unknown>" str
        (case r of
            (Right _)   -> assertBool "parser should reject this" False
            _           -> return ()
            )
        )

basic0 = parseEqTest "<a>" (Doc [Tag "a" [] Nothing])
basic1 = parseEqTest "<a<x:y>>" (Doc [Tag "a" [("x", [Str "y"])] Nothing])
basic2 = parseEqTest "<a<x:y><p:q>>" (Doc [
    Tag "a" [("x", [Str "y"]), ("p", [Str "q"])] Nothing])
basic3 = parseEqTest "<a<x:y><p:q>:>" (Doc [
    Tag "a" [("x", [Str "y"]), ("p", [Str "q"])] (Just [])])
basic4 = parseEqTest "<a<x:y><p:q>:a>" (Doc [
    Tag "a" [("x", [Str "y"]), ("p", [Str "q"])] (Just [
        Str "a"
    ])])
basic5 = parseEqTest "<a<x:y><p:q>:a<b>>" (Doc [
    Tag "a" [("x", [Str "y"]), ("p", [Str "q"])] (Just [
        Str "a", Tag "b" [] Nothing
    ])])
basic6 = parseEqTest "<a<x:y><p:q>:a<b>c>" (Doc [
    Tag "a" [("x", [Str "y"]), ("p", [Str "q"])] (Just [
        Str "a", Tag "b" [] Nothing, Str "c"
    ])])
basic7 = parseEqTest "<(--a<--b):\\>><\\ :\\>>:a\\<b\\>c>" (Doc [
    Tag "(--a" [("--b)", [Str ">"]), (" ", [Str ">"])] (Just [
        Str "a<b>c"
    ])])

reject0 = parseRejectTest "<"
reject1 = parseRejectTest ">"
reject2 = parseRejectTest ":"
reject3 = parseRejectTest "<>>"
reject4 = parseRejectTest "<<<>>"
reject5 = parseRejectTest "<:>"
reject6 = parseRejectTest "<a::>"
reject7 = parseRejectTest "a\\"

space0 = parseEqTest " a " (Doc [Str "a"])
space1 = parseEqTest " a b " (Doc [Str "a b"])
space2 = parseEqTest " a b   " (Doc [Str "a b"])
space3 = parseEqTest " a   b   " (Doc [Str "a b"])
space4 = parseEqTest "    a   b   " (Doc [Str "a b"])
space5 = parseEqTest "    a  \\ b   " (Doc [Str "a  b"])
space6 = parseEqTest "    a \\  b   " (Doc [Str "a   b"])
space7 = parseEqTest "    a \\ <x> b   " (Doc [
    Str "a  ", Tag "x" [] Nothing, Str "b"])
space8 = parseEqTest "\\    a \\ <x> b   " (Doc [
    Str "  a  ", Tag "x" [] Nothing, Str "b"])
space9 = parseEqTest "\\ \\\\   a \\ <x> b   " (Doc [
    Str " \\ a  ", Tag "x" [] Nothing, Str "b"])
space10 = parseEqTest "a\\ " (Doc [Str "a "])

tests = TestList [
    TestLabel "basic0" basic0,
    TestLabel "basic1" basic1 ,
    TestLabel "basic2" basic2,
    TestLabel "basic3" basic3,
    TestLabel "basic4" basic4,
    TestLabel "basic5" basic5,
    TestLabel "basic6" basic6,
    TestLabel "basic7" basic7,
    TestLabel "reject0" reject0,
    TestLabel "reject1" reject1,
    TestLabel "reject2" reject3,
    TestLabel "reject3" reject3,
    TestLabel "reject4" reject4,
    TestLabel "reject5" reject5,
    TestLabel "reject6" reject6,
    TestLabel "reject7" reject7,
    TestLabel "space0" space0,
    TestLabel "space1" space1,
    TestLabel "space2" space2,
    TestLabel "space3" space3,
    TestLabel "space4" space4,
    TestLabel "space5" space5,
    TestLabel "space6" space6,
    TestLabel "space7" space7,
    TestLabel "space8" space8,
    TestLabel "space9" space9,
    TestLabel "space10" space10
    ]

main :: IO ()
main = do
    r <- runTestTT $ tests
    if failures r > 0 then System.Exit.exitFailure else return ()
