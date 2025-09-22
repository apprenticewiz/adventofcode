module Main ( main ) where

import Data.Int (Int32)
import Data.List (group)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, readFile, stderr)

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

main :: IO ()
main = do
    args <- getArgs
    progname <- getProgName
    case args of
        [inputStr] -> do
            let result = process inputStr
            putStrLn $ "result = " ++ show result
        _ -> usage progname
