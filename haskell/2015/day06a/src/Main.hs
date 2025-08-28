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

perform :: Array (Int, Int) Bool -> Action -> (Int, Int) -> (Int, Int) -> Array (Int, Int) Bool
perform grid action upperLeft lowerRight =
    let (r1, c1) = upperLeft
        (r2, c2) = lowerRight
    in case action of
        TurnOn -> grid Array.// [((row, col), True) | row <- [r1..r2], col <- [c1..c2]]
        TurnOff -> grid Array.// [((row, col), False) | row <- [r1..r2], col <- [c1..c2]]
        Toggle -> grid Array.// [((row, col), not (grid Array.! (row, col))) | row <- [r1..r2], col <- [c1..c2]]

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
                (Array.array ((0, 0), (999, 999)) [((row, col), False) | row <- [0..999], col <- [0..999]])
                (lines content)
    in fromIntegral $ length $ filter id (Array.elems finalGrid)

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
