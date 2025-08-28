module Main ( main ) where

import Data.Array as Array
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, readFile, stderr)
import Data.Int (Int32)

data Action = TurnOn | TurnOff | Toggle

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

perform :: Array (Int, Int) Int -> Action -> (Int, Int) -> (Int, Int) -> Array (Int, Int) Int
perform grid action upperLeft lowerRight =
    let (r1, c1) = upperLeft
        (r2, c2) = lowerRight
    in case action of
        TurnOn -> grid Array.// [((row, col), (grid Array.! (row, col)) + 1) | row <- [r1..r2], col <- [c1..c2]]
        TurnOff -> grid Array.// [((row, col), (maximum [0, grid Array.! (row, col) - 1])) | row <- [r1..r2], col <- [c1..c2]]
        Toggle -> grid Array.// [((row, col), (grid Array.! (row, col)) + 2) | row <- [r1..r2], col <- [c1..c2]]

process :: String -> Int32
process content =
    let finalGrid = 
            foldl
                (\currGrid line ->
                    let parts = words line
                        parseCoord s =
                          let (first,_:second) = span (\c -> c /= ',') s
                          in (read first :: Int, read second :: Int)
                    in case parts of
                         ["turn", "on", upperLeft, "through", lowerRight] ->
                             perform currGrid TurnOn (parseCoord upperLeft) (parseCoord lowerRight)
                         ["turn", "off", upperLeft, "through", lowerRight] ->
                             perform currGrid TurnOff (parseCoord upperLeft) (parseCoord lowerRight)
                         ["toggle", upperLeft, "through", lowerRight] ->
                             perform currGrid Toggle (parseCoord upperLeft) (parseCoord lowerRight)
                         _ -> error ("malformed input line: " ++ line)

                )
                (Array.array ((0, 0), (999, 999)) [((row, col), 0) | row <- [0..999], col <- [0..999]])
                (lines content)
    in fromIntegral $ sum (Array.elems finalGrid)

main :: IO ()
main = do
    args <- getArgs
    progname <- getProgName
    case args of
        [filename] -> do
            content <- readFile filename
            let result = process content
            putStrLn $ "result = " ++ show result
        _ -> usage progname
