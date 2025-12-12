module Main ( main ) where

import Control.DeepSeq
import Data.Bits
import Data.List
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import System.Clock
import System.Environment
import System.Exit
import System.IO
import Text.Parsec
import Text.Parsec.String

type Shape = [(Int, Int)]

type Bitmask = Integer

data Region = Region { width :: Int, height :: Int, counts :: [Int] }
    deriving (Show)

{-# INLINE toBitPos #-}
toBitPos :: Int -> Int -> Int -> Int
toBitPos w r c = r * w + c

{-# INLINE shapeToBitmask #-}
shapeToBitmask :: Int -> Shape -> Int -> Int -> Bitmask
shapeToBitmask w shape r c =
    foldl' (\mask (dr, dc) -> 
        setBit mask (toBitPos w (r + dr) (c + dc))) 
        0 shape

{-# INLINE isValidPlacement #-}
isValidPlacement :: Int -> Int -> Bitmask -> Shape -> Int -> Int -> Bool
isValidPlacement w h occupied shape r c =
    all (\(dr, dc) ->
        let nr = r + dr
            nc = c + dc
        in nr >= 0 && nr < h && nc >= 0 && nc < w &&
           not (testBit occupied (toBitPos w nr nc))
    ) shape

integer :: Parser Int
integer = read <$> many1 digit

shapeParser :: Parser (Int, Shape)
shapeParser = do
    shapeIdx <- integer
    _ <- char ':'
    _ <- newline
    gridLines <- many1 (try (many1 (oneOf ".#") <* newline))
    let coords = [(r, c) | (r, row) <- zip [0..] gridLines, 
                           (c, ch) <- zip [0..] row, ch == '#']
    return (shapeIdx, coords)

isRegionLine :: Parser ()
isRegionLine = try $ do
    _ <- many1 digit
    _ <- char 'x'
    _ <- many1 digit
    _ <- char ':'
    return ()

shapesParser :: Parser [(Int, Shape)]
shapesParser = manyTill (shapeParser <* many newline) (lookAhead isRegionLine)

regionParser :: Parser Region
regionParser = do
    w <- integer
    _ <- char 'x'
    h <- integer
    _ <- char ':'
    _ <- many1 (char ' ')
    cnts <- integer `sepBy1` many1 (char ' ')
    return $ Region w h cnts

regionsParser :: Parser [Region]
regionsParser = regionParser `sepEndBy` newline

inputParser :: Parser (Map Int Shape, [Region])
inputParser = do
    shapesList <- shapesParser
    regions <- regionsParser
    _ <- many newline
    eof
    return (Map.fromList shapesList, regions)

parseInput :: String -> Either ParseError (Map Int Shape, [Region])
parseInput = parse inputParser ""

transformations :: Shape -> [Shape]
transformations shape =
    let rot90 s = [(-c, r) | (r, c) <- s]
        flipH s = [(-r, c) | (r, c) <- s]
        normalizeShape s = 
            let minR = minimum (map fst s)
                minC = minimum (map snd s)
            in sortBy (\(r1,c1) (r2,c2) -> compare (r1,c1) (r2,c2)) 
                      [(r - minR, c - minC) | (r, c) <- s]
        rots = take 4 $ iterate rot90 shape
        allTrans = rots ++ map flipH rots
        normalized = map normalizeShape allTrans
    in Map.elems $ Map.fromList [(s, s) | s <- normalized]

tryPlace :: Int -> Int -> Map Int Shape -> [Int] -> Bitmask -> Bool
tryPlace w h shapes presentCounts !occupied =
    let countsList = [(idx, c) | (idx, c) <- zip [0..] presentCounts, c > 0]
        totalArea = w * h
        requiredArea = sum [c * length s | (idx, c) <- countsList, Just s <- [Map.lookup idx shapes]]
        transCache = Map.fromList [(idx, transformations shape) 
                                  | (idx, c) <- countsList, c > 0,
                                    Just shape <- [Map.lookup idx shapes]]
    in requiredArea <= totalArea && tryPlaceHelper w h countsList occupied transCache
  where
    tryPlaceHelper :: Int -> Int -> [(Int, Int)] -> Bitmask -> Map Int [Shape] -> Bool
    tryPlaceHelper !_ !_ [] !_ _ = True
    tryPlaceHelper !w' !h' ((shapeIdx, cnt):restCounts) !occ transCache =
        case Map.lookup shapeIdx transCache of
            Nothing -> tryPlaceHelper w' h' restCounts occ transCache
            Just orientations ->
                any (\orientation ->
                    any (\(r, c) ->
                        isValidPlacement w' h' occ orientation r c &&
                        let !placementMask = shapeToBitmask w' orientation r c
                            !occ' = occ .|. placementMask
                            restCounts' = if cnt > 1 
                                          then (shapeIdx, cnt - 1) : restCounts
                                          else restCounts
                        in tryPlaceHelper w' h' restCounts' occ' transCache
                    ) [(r, c) | r <- [0..h'-1], c <- [0..w'-1]]
                ) orientations

canFitRegion :: Map Int Shape -> Region -> Bool
canFitRegion shapes (Region w h presentCounts) =
    tryPlace w h shapes presentCounts 0

process :: String -> Int
process content = 
    case parseInput content of
        Left err -> error (show err)
        Right (shapes, regions) ->
            let validRegions = filter (canFitRegion shapes) regions
            in length validRegions

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

