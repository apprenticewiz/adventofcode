module Main ( main ) where

import Control.Monad (forM_)
import Control.Monad.ST (ST)
import Data.Array.ST (STUArray, newArray, readArray, runSTUArray, writeArray)
import Data.Array.Unboxed (elems)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Data.Int (Int32)
import Data.Word (Word8)
import Control.DeepSeq
import System.Clock

data Action = TurnOn | TurnOff | Toggle

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

perform :: STUArray s (Int, Int) Word8 -> Action -> (Int, Int) -> (Int, Int) -> ST s ()
perform grid action (r1, c1) (r2, c2) =
    forM_ [r1..r2] $ \row ->
        forM_ [c1..c2] $ \col -> do
            old <- readArray grid (row, col)
            let newVal = case action of
                             TurnOn -> old + 1
                             TurnOff -> if old > 0 then old - 1 else old
                             Toggle -> old + 2
            writeArray grid (row, col) newVal

process :: String -> Int32
process content =
    let final = runSTUArray $ do
                grid <- newArray ((0, 0), (999, 999)) 0
                forM_ (lines content) $ \line ->
                    let parts = words line
                        parseCoord s = case span (/= ',') s of
                                           (first, _:second) -> (read first, read second)
                                           _ -> error ("malformed coordinate in line: " ++ line)
                    in case parts of
                        ["turn", "on", ul, "through", lr] -> perform grid TurnOn (parseCoord ul) (parseCoord lr)
                        ["turn", "off", ul, "through", lr] -> perform grid TurnOff (parseCoord ul) (parseCoord lr)
                        ["toggle", ul, "through", lr] -> perform grid Toggle (parseCoord ul) (parseCoord lr)
                        _ -> error ("malformed input line: " ++ line)
                return grid
    in sum $ map fromIntegral (elems final)


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
