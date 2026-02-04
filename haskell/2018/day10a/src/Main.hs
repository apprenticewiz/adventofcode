module Main ( main ) where

import Control.Monad
import System.Environment
import System.Exit
import System.IO
import Text.Parsec
import Text.Parsec.String
import Control.DeepSeq
import System.Clock

type Light = ((Int, Int), (Int, Int))

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

file :: Parser [Light]
file = line `sepEndBy1` newline <* eof

line :: Parser Light
line = do
    _ <- string "position="
    pos <- pair
    _ <- string " velocity="
    velo <- pair
    return (pos, velo)

pair :: Parser (Int, Int)
pair = do
    _ <- string "<" >> spaces
    x <- integer
    _ <- string "," >> spaces
    y <- integer
    _ <- string ">"
    return (x, y)

integer :: Parser Int
integer = read <$> ((++) <$> option "" (string "-") <*> many1 digit)

display :: Int -> [Light] -> IO ()
display currArea lights = do
    let nextLights = step lights
        nextArea = calcArea (boundingBox nextLights)
    if nextArea > currArea
        then do
            let positions = map fst lights
                ((minX, minY), (maxX, maxY)) = boundingBox lights
            forM_ [minY..maxY] $ \y -> do
                forM_ [minX..maxX] $ \x -> do
                    putStr [if (x, y) `elem` positions then '*' else ' ']
                putStr "\n"
        else display nextArea nextLights
  where
    step ls = [((x + dx, y + dy), (dx, dy)) | ((x, y), (dx, dy)) <- ls ]

    boundingBox ls =
        let minX = minimum $ map (fst . fst) ls
            minY = minimum $ map (snd . fst) ls
            maxX = maximum $ map (fst . fst) ls
            maxY = maximum $ map (snd . fst) ls
        in ((minX, minY), (maxX, maxY))

    calcArea ((minX, minY), (maxX, maxY)) = (maxX - minX) * (maxY - minY)

process :: String -> IO ()
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right lights -> display maxBound lights

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
            process content
            end <- getTime Monotonic
            let elapsed = diffTimeSpec start end
            putStrLn $ "elapsed time: " ++ showTime elapsed
        _ -> usage progname
