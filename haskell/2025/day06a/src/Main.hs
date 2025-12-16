module Main ( main ) where

import Control.DeepSeq
import Data.IntMap.Strict ( IntMap )
import qualified Data.IntMap.Strict as IntMap
import System.Clock
import System.Environment
import System.Exit
import System.IO

process :: String -> Int
process content =
    let ls      = lines content
        nums    = map (map read . words) (init ls)
        ops     = words (last ls)
        columns = foldr insertRow IntMap.empty nums
    in sum $ map (uncurry applyOp) (zip ops (IntMap.elems columns))

  where
    insertRow :: [Int] -> IntMap [Int] -> IntMap [Int]
    insertRow line cols =
        let row = IntMap.fromList (zip [0..] (map (:[]) line))
        in IntMap.unionWith (++) row cols

    applyOp :: String -> [Int] -> Int
    applyOp op ns =
        case op of
            "+" -> sum ns
            "*" -> product ns
            _   -> error "expected '+' or '*' for operator"

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

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
        [filename] -> do
            start <- getTime Monotonic
            content <- readFile filename
            let result = process content
            result `deepseq` return ()
            end <- getTime Monotonic
            let elapsed = diffTimeSpec start end
            putStrLn $ "result = " ++ show result
            putStrLn $ "elapsed time: " ++ showTime elapsed
        _ -> usage progname
