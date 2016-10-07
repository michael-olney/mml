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

roundTripTest e = TestCase (do
        let txt = unparse e
        r <- parse M.empty M.empty "<unknown>" txt
        (case r of
            (Right e2)   -> assertEqual "round trip test" e e2
            _            -> assertBool ("roundtrip rejection: " ++ txt) False
            )
        )

basic0 = parseEqTest "<a>" ([Tag "a" [] Nothing])
basic1 = parseEqTest "<a<x:y>>" ([Tag "a" [("x", [Str "y"])] Nothing])
basic2 = parseEqTest "<a<x:y><p:q>>" ([
    Tag "a" [("x", [Str "y"]), ("p", [Str "q"])] Nothing])
basic3 = parseEqTest "<a<x:y><p:q>:>" ([
    Tag "a" [("x", [Str "y"]), ("p", [Str "q"])] (Just [])])
basic4 = parseEqTest "<a<x:y><p:q>:a>" ([
    Tag "a" [("x", [Str "y"]), ("p", [Str "q"])] (Just [
        Str "a"
    ])])
basic5 = parseEqTest "<a<x:y><p:q>:a<b>>" ([
    Tag "a" [("x", [Str "y"]), ("p", [Str "q"])] (Just [
        Str "a", Tag "b" [] Nothing
    ])])
basic6 = parseEqTest "<a<x:y><p:q>:a<b>c>" ([
    Tag "a" [("x", [Str "y"]), ("p", [Str "q"])] (Just [
        Str "a", Tag "b" [] Nothing, Str "c"
    ])])
basic7 = parseEqTest "<(--a<--b):\\>><\\ :\\>>:a\\<b\\>c>" ([
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

space0 = parseEqTest " a " ([Str "a"])
space1 = parseEqTest " a b " ([Str "a b"])
space2 = parseEqTest " a b   " ([Str "a b"])
space3 = parseEqTest " a   b   " ([Str "a b"])
space4 = parseEqTest "    a   b   " ([Str "a b"])
space5 = parseEqTest "    a  \\ b   " ([Str "a  b"])
space6 = parseEqTest "    a \\  b   " ([Str "a   b"])
space7 = parseEqTest "    a \\ <x> b   " ([
    Str "a  ", Tag "x" [] Nothing, Str "b"])
space8 = parseEqTest "\\    a \\ <x> b   " ([
    Str "  a  ", Tag "x" [] Nothing, Str "b"])
space9 = parseEqTest "\\ \\\\   a \\ <x> b   " ([
    Str " \\ a  ", Tag "x" [] Nothing, Str "b"])
space10 = parseEqTest "a\\ " ([Str "a "])

roundtrip0 = roundTripTest [Tag "a" [] Nothing]
roundtrip1 = roundTripTest [Tag "a" [] (Just [Str "b"])]
-- TODO string separation..
--roundtrip2 = roundTripTest [Tag "a" [] (Just [Str "b", Str "c"])]
roundtrip3 = roundTripTest [Tag "a" [] (Just [Str "b", Tag "c" [] Nothing, Str "d"])]
roundtrip4 = roundTripTest [Tag "a" [] (Just [Str "b ", Tag "c" [] Nothing, Str " d "])]
roundtrip5 = roundTripTest [
        Tag "a" [("x", [Str "y"])]
        (Just [Str "b ", Tag "c" [] Nothing, Str " d "])
    ]
roundtrip6 = roundTripTest [
        Tag " a\\" [(">", [Str " <\\ "])]
        (Just [Str "b ", Tag "\\c " [] Nothing, Str " d "])
    ]
roundtrip7 = roundTripTest [
        Call " a\\" [Str "b ", Tag "\\c " [] Nothing, Str " d "]
    ]

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
    TestLabel "space10" space10,
    TestLabel "roundtrip0" roundtrip0,
    TestLabel "roundtrip1" roundtrip1,
    --TestLabel "roundtrip2" roundtrip2,
    TestLabel "roundtrip3" roundtrip3,
    TestLabel "roundtrip4" roundtrip4,
    TestLabel "roundtrip5" roundtrip5,
    TestLabel "roundtrip6" roundtrip6,
    TestLabel "roundtrip7" roundtrip7
    ]

main :: IO ()
main = do
    r <- runTestTT $ tests
    if failures r > 0 then System.Exit.exitFailure else return ()
