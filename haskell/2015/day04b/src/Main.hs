module Main ( main ) where

import Control.DeepSeq
import Data.Int (Int32)
import Data.List (isPrefixOf)
import Data.Word (Word8)
import Numeric (showHex)
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as ByteString
import System.Clock
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

showTime :: TimeSpec -> String
showTime elapsed =
    let ns = fromIntegral (toNanoSecs elapsed) :: Double
    in if ns < 1000
       then show ns ++ " ns"
       else if ns < 1000000
       then show (ns / 1000.0) ++ " μs"
       else if ns < 1000000000
            then show (ns / 1000000.0) ++ " ms"
            else show (ns / 1000000000.0) ++ " s"

main :: IO ()
main = do
    args <- getArgs
    progname <- getProgName
    case args of
        [key] -> do
            start <- getTime Monotonic
            let result = process key
            result `deepseq` return ()
            end <- getTime Monotonic
            let elapsed = diffTimeSpec start end
            putStrLn $ "result = " ++ show result
            putStrLn $ "elapsed time: " ++ showTime elapsed
        _ -> usage progname
