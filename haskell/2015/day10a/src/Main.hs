module Main ( main ) where

import Data.Int (Int32)
import Data.List (group)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, readFile, stderr)
import Control.DeepSeq
import System.Clock

type RLE = [(Char, Int)]

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <string>"
    exitFailure

encode :: String -> RLE
encode = map (\g -> (head g, length g)) . group

lookAndSay :: RLE -> RLE
lookAndSay rle = encode . concatMap f $ rle
  where
    f (digit, count) = show count ++ [digit]

process :: String -> Int32
process = (fromIntegral . iterateSteps 40)
  where
    iterateSteps n inputStr = totalLength (iterate lookAndSay (encode inputStr) !! n)
    totalLength = sum . map snd


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
        [inputStr] -> do
            start <- getTime Monotonic
            let result = process inputStr
            result `deepseq` return ()
            end <- getTime Monotonic
            let elapsed = diffTimeSpec start end
            putStrLn $ "result = " ++ show result
            putStrLn $ "elapsed time: " ++ showTime elapsed
        _ -> usage progname
