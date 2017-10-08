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

mkStr str = Str str dummySM
mkTag (Str name _) attrs children = Tag name attrs children dummySM
mkAttrs = M.fromList . (map (\(Str k _, v) -> (k, v)))

relaxedEq :: (Eq a, Data a) => a -> a -> Bool
relaxedEq x y = (norm x) == (norm y)
    where
        norm x = everywhere (mkT $ \_ -> dummySM) x

assertEqualRelaxed pref exp got = do
    let msg = pref ++ "Expected:\n" ++ (show exp)
                ++ "\n. Got:\n" ++ (show got) ++ "\n"
    assertBool msg (relaxedEq exp got)

tokenizeEqTest str v = TestCase (do
        r <- tokenize "<unknown>" str
        (case r of 
            (Left _)        -> assertBool "tokenizer should accept this" False
            (Right toks)    -> assertEqualRelaxed "" v (fst . unzip $ toks)
            )        
        )

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

roundTripTest e = roundTripTestFun e

basic0 = parseEqTest "{a}" ([mkTag (mkStr "a") (M.empty) Nothing])
basic1 = parseEqTest "{a{x→y}}" ([
    mkTag (mkStr "a") (mkAttrs [(mkStr "x", [mkStr "y"])]) Nothing])
basic2 = parseEqTest "{a{x→y}{p→q}}" ([
    mkTag (mkStr "a") (mkAttrs [(mkStr "x", [mkStr "y"]), (mkStr "p", [mkStr "q"])]) Nothing])
basic3 = parseEqTest "{a{x→y}{p→q}→}" ([
    mkTag (mkStr "a") (mkAttrs [(mkStr "x", [mkStr "y"]), (mkStr "p", [mkStr "q"])]) (Just [])])
basic4 = parseEqTest "{a{x→y}{p→q}→a}" ([
    mkTag (mkStr "a") (mkAttrs [(mkStr "x", [mkStr "y"]), (mkStr "p", [mkStr "q"])]) (Just [
        mkStr "a"
    ])])
basic5 = parseEqTest "{a{x→y}{p→q}→a{b}}" ([
    mkTag (mkStr "a") (mkAttrs [(mkStr "x", [mkStr "y"]), (mkStr "p", [mkStr "q"])]) (Just [
        mkStr "a", mkTag (mkStr "b") (M.empty) Nothing
    ])])
basic6 = parseEqTest "{a{x→y}{p→q}→a{b}c}" ([
    mkTag (mkStr "a") (mkAttrs [(mkStr "x", [mkStr "y"]), (mkStr "p", [mkStr "q"])]) (Just [
        mkStr "a", mkTag (mkStr "b") M.empty Nothing, mkStr "c"
    ])])
basic7 = parseEqTest "{(--a{--b)→\\}}{\\ →\\}}→a\\{b\\}c}" ([
    mkTag (mkStr "(--a") (mkAttrs [(mkStr "--b)", [mkStr "}"]), (mkStr " ", [mkStr "}"])]) (Just [
        mkStr "a{b}c"
    ])])
basic11 = parseEqTest "1. This is a  test" [mkStr "1. This is a test"]
basic12 = parseEqTest "{t→a b}" [mkTag (mkStr "t") M.empty (Just [mkStr "a b"])]

reject0 = parseRejectTest "{"
reject1 = parseRejectTest "}"
reject2 = parseRejectTest ":"
reject3 = parseRejectTest "{}}"
reject4 = parseRejectTest "{{{}}"
reject5 = parseRejectTest "{→}"
reject6 = parseRejectTest "{a→→}"
reject7 = parseRejectTest "a\\"

space0 = parseEqTest " a " ([mkStr "a"])
space1 = parseEqTest " a b " ([mkStr "a b"])
space2 = parseEqTest " a b   " ([mkStr "a b"])
space3 = parseEqTest " a   b   " ([mkStr "a b"])
space4 = parseEqTest "    a   b   " ([mkStr "a b"])
space5 = parseEqTest "    a  \\ b   " ([mkStr "a b"])
space6 = parseEqTest "    a \\  b   " ([mkStr "a b"])
space7 = parseEqTest "    a \\ {x} b   " ([
    mkStr "a ", mkTag (mkStr "x") M.empty Nothing, mkStr "b"])
space8 = parseEqTest "\\    a \\ {x} b   " ([
    mkStr " a ", mkTag (mkStr "x") M.empty Nothing, mkStr "b"])
space9 = parseEqTest "\\ \\\\   a \\ {x} b   " ([
    mkStr " \\ a ", mkTag (mkStr "x") M.empty Nothing, mkStr "b"])
space10 = parseEqTest "a\\ " ([mkStr "a "])

roundtrip0 = roundTripTest [mkTag (mkStr "a") M.empty Nothing]
roundtrip1 = roundTripTest [mkTag (mkStr "a") M.empty (Just [mkStr "b"])]
roundtrip2 = roundTripTest [mkTag (mkStr "a") M.empty (Just [mkStr "b", mkStr "c"])]
roundtrip3 = roundTripTest [mkTag (mkStr "a") M.empty (Just [mkStr "b", mkTag (mkStr "c") M.empty Nothing, mkStr "d"])]
roundtrip4 = roundTripTest [mkTag (mkStr "a") M.empty (Just [mkStr "b ", mkTag (mkStr "c") M.empty Nothing, mkStr " d "])]
roundtrip5 = roundTripTest [
        mkTag (mkStr "a") (mkAttrs [(mkStr "x", [mkStr "y"])])
        (Just [mkStr "b ", mkTag (mkStr "c") M.empty Nothing, mkStr " d "])
    ]
roundtrip6 = roundTripTest [
        mkTag (mkStr " a\\") (mkAttrs [(mkStr "}", [mkStr " {\\ "])])
        (Just [mkStr "b ", mkTag (mkStr "\\c ") M.empty Nothing, mkStr " d "])
    ]
roundtrip8 = roundTripTest [mkStr " "]
roundtrip9 = roundTripTest [mkStr ""]
roundtrip13 = roundTripTest [mkStr "1. This is a  test"]

stringsep0 = parseEqTest " a ~ b " [mkStr "a", mkStr "b"]
stringsep1 = parseRejectTest " a ~~ b "
stringsep2 = parseRejectTest "~a"

tokenize1 = tokenizeEqTest "`  {→~^`" [
        TChar ' ',
        TChar ' ',
        TChar '{',
        TChar '→',
        TChar '~',
        TChar '^',
        TEOF
        ]

tokenize2 = tokenizeEqTest " ^ " [
        TEmptyStr,
        TEOF
        ]

tokenize3 = tokenizeEqTest "/*` {*/abc" [
        TChar 'a',
        TChar 'b',
        TChar 'c',
        TEOF
        ]

tokenize5 = tokenizeEqTest " ~ " [
        TStrSep,
        TEOF
        ]
tokenize6 = tokenizeEqTest " → " [
        TSplit,
        TEOF
        ]
tokenize7 = tokenizeEqTest "\\}" [
        TChar '}',
        TEOF
        ]
tokenize8 = tokenizeEqTest "\\\\" [
        TChar '\\',
        TEOF
        ]
tokenize9 = tokenizeEqTest " a b " [
        TChar 'a',
        TSpace Nothing,
        TChar 'b',
        TEOF
        ]
tokenize10 = tokenizeEqTest " a\\ b " [
        TChar 'a',
        TSpace (Just ' '),
        TChar 'b',
        TEOF
        ]
tokenize11 = tokenizeEqTest " a\\  b " [
        TChar 'a',
        TSpace (Just ' '),
        TChar 'b',
        TEOF
        ]
tokenize12 = tokenizeEqTest " a \\  b " [
        TChar 'a',
        TSpace (Just ' '),
        TChar 'b',
        TEOF
        ]
tokenize13 = tokenizeEqTest " a \\ \\ b " [
        TChar 'a',
        TSpace (Just ' '),
        TSpace (Just ' '),
        TChar 'b',
        TEOF
        ]
tokenize14 = tokenizeEqTest " < \\ \\ > " [
        TBrace BTTag BDOpen BVCharLike,
        TSpace (Just ' '),
        TSpace (Just ' '),
        TBrace BTUnknown BDClose BVCharLike,
        TEOF
        ]
tokenize17 = tokenizeEqTest "{t→a b}" [
        TBrace BTTag BDOpen BVSpecialLike,
        TChar 't',
        TSplit,
        TChar 'a',
        TSpace Nothing,
        TChar 'b',
        TBrace BTUnknown BDClose BVSpecialLike,
        TEOF
        ]
tokenize18 = tokenizeEqTest "{t→a b }" [
        TBrace BTTag BDOpen BVSpecialLike,
        TChar 't',
        TSplit,
        TChar 'a',
        TSpace Nothing,
        TChar 'b',
        TBrace BTUnknown BDClose BVSpecialLike,
        TEOF
        ]
tokenize19 = tokenizeEqTest "<t→a b \n >  c" [
        TBrace BTTag BDOpen BVCharLike,
        TChar 't',
        TSplit,
        TChar 'a',
        TSpace Nothing,
        TChar 'b',
        TBrace BTUnknown BDClose BVCharLike,
        TSpace Nothing,
        TChar 'c',
        TEOF
        ]

tests = TestList [
    TestLabel "tokenize1" tokenize1,
    TestLabel "tokenize2" tokenize2,
    TestLabel "tokenize3" tokenize3,
    TestLabel "tokenize5" tokenize5,
    TestLabel "tokenize6" tokenize6,
    TestLabel "tokenize7" tokenize7,
    TestLabel "tokenize8" tokenize8,
    TestLabel "tokenize9" tokenize9,
    TestLabel "tokenize10" tokenize10,
    TestLabel "tokenize11" tokenize11,
    TestLabel "tokenize12" tokenize12,
    TestLabel "tokenize13" tokenize13,
    TestLabel "tokenize14" tokenize14,
    TestLabel "tokenize17" tokenize17,
    TestLabel "tokenize18" tokenize18,
    TestLabel "tokenize19" tokenize19,
    TestLabel "basic0" basic0,
    TestLabel "basic1" basic1,
    TestLabel "basic2" basic2,
    TestLabel "basic3" basic3,
    TestLabel "basic4" basic4,
    TestLabel "basic5" basic5,
    TestLabel "basic6" basic6,
    TestLabel "basic7" basic7,
    TestLabel "basic11" basic11,
    TestLabel "basic12" basic12,
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
    TestLabel "roundtrip2" roundtrip2,
    TestLabel "roundtrip3" roundtrip3,
    TestLabel "roundtrip4" roundtrip4,
    TestLabel "roundtrip5" roundtrip5,
    TestLabel "roundtrip6" roundtrip6,
    --TestLabel "roundtrip7" roundtrip7,
    TestLabel "roundtrip8" roundtrip8,
    TestLabel "roundtrip9" roundtrip9,
    TestLabel "roundtrip13" roundtrip13,
    TestLabel "stringsep0" stringsep0,
    TestLabel "stringsep1" stringsep1,
    TestLabel "stringsep2" stringsep2
    ]

main :: IO ()
main = do
    r <- runTestTT $ tests
    if failures r > 0 then System.Exit.exitFailure else return ()
