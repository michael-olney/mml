{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import MML
import MML.Funs

import MML.Format
import MML.Format.JSON
import MML.Format.HTML
import MML.Format.MML
import MML.Format (Format)

import System.IO
import System.Environment
import System.FilePath.Posix
import System.Exit (exitWith, ExitCode(..))

import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BS
import Data.Char (toUpper)

import qualified Options.Applicative as O
import Options.Applicative ((<**>))
import Data.Semigroup

import Text.Read (readMaybe)

die :: String -> IO a
die msg = do
    hPutStrLn stderr msg
    exitWith (ExitFailure 1)

data Options = Options {
    infile :: String,
    infmt :: Format,
    outfile :: String,
    outfmt :: Format
}
    deriving (Eq, Ord, Show, Read)

readFmt :: String -> Either String Format
readFmt str =
    case (readMaybe . (map toUpper) $ str) of
        Nothing     -> Left "invalid format type"
        (Just fmt)  -> Right fmt

optParser :: O.Parser Options
optParser =
    Options
    <$> O.strOption (
        O.long "infile"
        <> O.metavar "INFILE"
        <> O.help "Input file."
        )
    <*> O.option (O.eitherReader readFmt) (
        O.long "infmt"
        <> O.metavar "INFMT"
        <> O.help "Format of input file."
        )
    <*> O.strOption (
        O.long "outfile"
        <> O.metavar "OUTFILE"
        <> O.help "Output file."
        )
    <*> O.option (O.eitherReader readFmt) (
        O.long "outfmt"
        <> O.metavar "OUTFMT"
        <> O.help "Format of output file."
        )

convsIn :: M.Map Format (String -> BS.ByteString -> IO (Either String Doc))
convsIn = M.fromList [
    (MML, fromMML)
    ]

convsOut :: M.Map Format (Doc -> IO (Either String BS.ByteString))
convsOut = M.fromList [
    (HTML, toHTML),
    (JSON, toJSON)
    ]

tryGetConv map fmt prep = (case M.member fmt map of
    True    -> return $ map M.! fmt
    False   -> die $ "No conversion " ++ prep ++ " " ++ (show fmt)
    )

mustRight pref (Left err)   = die $ pref ++ ": " ++ err
mustRight pref (Right x)    = return x

convert :: String -> BS.ByteString -> Format -> Handle -> Format -> IO ()
convert infile inp infmt oh outfmt = do
    convIn <- tryGetConv convsIn infmt "from"
    convOut <- tryGetConv convsOut outfmt "to"

    internal <- convIn infile inp >>= mustRight "input conversion error"
    outp <- convOut internal >>= mustRight "output conversion error"

    BS.hPut oh outp

process :: Options -> IO ()
process (Options {
        infile=infile, infmt=infmt,
        outfile=outfile, outfmt=outfmt}) = do

    ih <- openBinaryFile infile ReadMode
    oh <- openBinaryFile outfile WriteMode

    inp <- BS.hGetContents ih
    convert infile inp infmt oh outfmt

    hClose oh
    hClose ih

main = process =<< O.execParser opts
    where
        opts = O.info
            (optParser <**> O.helper)
            (
                O.fullDesc
                <> O.progDesc "Convert INFILE to OUTFMT"
                <> O.header "mml - Convert to and from MML"
                )

