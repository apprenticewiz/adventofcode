module Main ( main ) where

import System.Environment
import System.Exit
import System.IO
import Text.Parsec
import Text.Parsec.String
import Control.DeepSeq
import System.Clock

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

file :: Parser [((Int, Int, Int), (Int, Int, Int), (Int, Int, Int))]
file = line `sepEndBy1` newline <* eof

line :: Parser ((Int, Int, Int), (Int, Int, Int), (Int, Int, Int))
line = do
    p <- parseTriple "p"
    _ <- string ", "
    v <- parseTriple "v"
    _ <- string ", "
    a <- parseTriple "a"
    return (p, v, a)

parseTriple :: String -> Parser (Int, Int, Int)
parseTriple x = do
    _ <- string (x ++ "=<")
    n1 <- parseInt
    _ <- string ","
    n2 <- parseInt
    _ <- string ","
    n3 <- parseInt
    _ <- string ">"
    return (n1, n2, n3)

parseInt :: Parser Int
parseInt = read <$> ((++) <$> option "" (string "-") <*> many1 digit)

manhattan :: (Int, Int, Int) -> Int
manhattan (x, y, z) = abs x + abs y + abs z

process :: String -> Int
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right particles ->
            let (_, _, _, closest, _) = foldl' step (maxBound, maxBound, maxBound, -1, 0) particles
            in closest
  where
    step (minP, minV, minA, closest, n) particle =
        let (p, v, a) = particle
            (distP, distV, distA) = (manhattan p, manhattan v, manhattan a)
        in if distA < minA || (distA == minA && distV < minV) || (distA == minA && distV == minV && distP < minP )
            then (distP, distV, distA, n, n + 1)
            else (minP, minV, minA, closest, n + 1)


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
