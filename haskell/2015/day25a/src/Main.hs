module Main ( main ) where

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, readFile, stderr)
import Text.Parsec
import Text.Parsec.String
import Control.DeepSeq
import System.Clock

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

file :: Parser (Int, Int)
file = do
       _ <- string "To continue, please consult the code grid in the manual.  Enter the code at row "
       row <- many1 digit
       _ <- string ", column "
       col <- many1 digit
       _ <- string "."
       optional newline >> eof
       return (read row, read col)

generate :: (Int, Int) -> Integer
generate (r, c) 
  | r == 1 && c == 1 = 20151125 :: Integer
  | r > 1 && c == 1  = step (generate (1, r - 1))
  | otherwise        = step (generate (r + 1, c - 1))
  where
    step n = (n * 252533) `mod` 33554393

process :: String -> Integer
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right (row, col) -> generate (row, col)


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
