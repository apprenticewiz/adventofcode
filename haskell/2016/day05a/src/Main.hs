module Main ( main ) where

import Control.DeepSeq
import Control.Monad
import Control.Monad.State
import Data.Char (ord)
import Data.List (isPrefixOf)
import Data.Word (Word8)
import Numeric (showHex)
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as ByteString
import System.Clock
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <key>"
    exitFailure

genPassword :: Int -> String -> State (Int, String) String
genPassword len base =
    do
    forM_ [1..len] $ \_ -> do
        (n, password) <- get
        let (n', md5hash) = findNext (n + 1) base
        put (n', password ++ [md5hash !! 5])
    (_, finalPassword) <- get
    return finalPassword
  where
    findNext :: Int -> String -> (Int, String)
    findNext i s =
        let keyBytes = ByteString.pack $ map (fromIntegral . ord) (s ++ show i)
            digestBytes = MD5.hash keyBytes
            digest = toHex $ ByteString.unpack digestBytes
        in if "00000" `isPrefixOf` digest
            then (i, digest)
            else findNext (i + 1) s

    toHex :: [Word8] -> String
    toHex = foldl (\acc x -> let hexValue = showHex x ""
                             in if length hexValue == 1
                                then acc ++ "0" ++ hexValue
                                else acc ++ hexValue
                  ) ""

process :: String -> String
process base = evalState (genPassword 8 base) (1, "")

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
        [base] -> do
            start <- getTime Monotonic
            let result = process base
            result `deepseq` return ()
            end <- getTime Monotonic
            let elapsed = diffTimeSpec start end
            putStrLn $ "result = " ++ show result
            putStrLn $ "elapsed time: " ++ showTime elapsed
        _ -> usage progname
