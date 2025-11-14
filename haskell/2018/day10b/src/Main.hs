module Main ( main ) where

import System.Environment
import System.Exit
import System.IO
import Text.Parsec
import Text.Parsec.String

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

calcTime :: Int -> [Light] -> Int -> Int
calcTime currArea lights time =
    let nextLights = step lights
        nextArea = calcArea (boundingBox nextLights)
    in if nextArea > currArea
        then time
        else calcTime nextArea nextLights (time + 1)
  where
    step ls = [((x + dx, y + dy), (dx, dy)) | ((x, y), (dx, dy)) <- ls ]

    boundingBox ls =
        let minX = minimum $ map (fst . fst) ls
            minY = minimum $ map (snd . fst) ls
            maxX = maximum $ map (fst . fst) ls
            maxY = maximum $ map (snd . fst) ls
        in ((minX, minY), (maxX, maxY))

    calcArea ((minX, minY), (maxX, maxY)) = (maxX - minX) * (maxY - minY)

process :: String -> Int
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right lights -> calcTime maxBound lights 0

main :: IO ()
main = do
    args <- getArgs
    progname <- getProgName
    case args of
        [filename] -> do
            content <- readFile filename
            let result = process content
            putStrLn ("result = " ++ show result)
        _ -> usage progname
