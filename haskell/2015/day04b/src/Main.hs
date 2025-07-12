module Main ( main ) where

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as ByteString
import Data.Int (Int32)
import Data.Word (Word8)
import Data.List (isPrefixOf)
import Numeric (showHex)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, readFile, stderr)

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <key>"
    exitFailure

process :: String -> Int32
process = check 1
    where
        toHex :: [Word8] -> String
        toHex = foldl
                    (\acc x ->
                        let hexValue = showHex x ""
                        in if length hexValue == 1
                            then acc ++ ("0" ++ hexValue)
                            else acc ++ hexValue
                    )
                    ""
        check n key =
            let keyBytes = ByteString.pack $ Prelude.map (fromIntegral . fromEnum) (key ++ show n)
                digestBytes = MD5.hash keyBytes
                digest = toHex $ ByteString.unpack digestBytes
            in if "000000" `isPrefixOf` digest
                then n
                else check (n + 1) key

main :: IO ()
main = do
    args <- getArgs
    progname <- getProgName
    case args of
        [key] -> do
            let result = process key
            putStrLn $ "result = " ++ show result
        _ -> usage progname
