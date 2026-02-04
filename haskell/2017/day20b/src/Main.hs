module Main ( main ) where

import Control.DeepSeq
import qualified Data.Map.Strict as Map
import System.Clock
import System.Environment
import System.Exit
import System.IO
import Text.Parsec
import Text.Parsec.String

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

dot :: (Int, Int, Int) -> (Int, Int, Int) -> Int
dot (x1, y1, z1) (x2, y2, z2) = (x1 * x2) + (y1 * y2) + (z1 * z2)

process :: String -> Int
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right particles ->
            let finalParticles = loop particles
            in length finalParticles
  where
    loop particles =
        let particles' = map (\(p, (vx, vy, vz), (ax, ay, az)) -> (p, (vx + ax, vy + ay, vz + az), (ax, ay, az))) particles
            particles'' = map (\((px, py, pz), (vx, vy, vz), a) -> ((px + vx, py + vy, pz + vz), (vx, vy, vz), a)) particles'
            positions = foldl' (\ps (n, ((px, py, pz), _, _)) -> Map.insertWith (++) (px, py, pz) [n] ps)
                Map.empty (zip [0..] particles'')
            collisions = Map.filter (\x -> length x > 1) positions
            collidedParticles = concat (Map.elems collisions)
            particles''' = map snd $ filter (\(n, _) -> n `notElem` collidedParticles) (zip [0..] particles'')
            dots = map (\(p, v, _) -> dot p v) particles'''
        in if all (> 0) dots
            then particles'''
            else loop particles'''

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
