module Main ( main ) where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import System.Environment
import System.Exit
import System.IO
import Text.Parsec
import Text.Parsec.String

data Operation = Rect Int Int
               | RotateRow Int Int
               | RotateCol Int Int
               deriving (Eq, Show)

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

file :: Parser [Operation]
file = (line `sepEndBy` newline) <* eof

line :: Parser Operation
line = try rect <|> try rotateRow <|> rotateCol

rect :: Parser Operation
rect = do
    _ <- string "rect "
    xStr <- many1 digit
    _ <- string "x"
    yStr <- many1 digit
    return $ Rect (read xStr) (read yStr)

rotateRow :: Parser Operation
rotateRow = do
    _ <- string "rotate row y="
    rowStr <- many1 digit
    _ <- string " by "
    amtStr <- many1 digit
    return $ RotateRow (read rowStr) (read amtStr)

rotateCol :: Parser Operation
rotateCol = do
    _ <- string "rotate column x="
    colStr <- many1 digit
    _ <- string " by "
    amtStr <- many1 digit
    return $ RotateCol (read colStr) (read amtStr)

upperBound :: (Int, Int)
upperBound = (49, 5)

applyOps :: [Operation] -> UArray (Int, Int) Int
applyOps ops = runST $ do
   grid <- newArray ((0, 0), upperBound) 0 :: ST s (STUArray s (Int, Int) Int)
   forM_ ops $ \op ->
       case op of
           Rect x y ->
               forM_ [0..(x - 1)] $ \i ->
                   forM_ [0..(y - 1)] $ \j -> do
                       writeArray grid (i, j) 1
           RotateRow y amt -> do
               es <- forM [0..fst upperBound] $ \i -> readArray grid (i, y)
               let n = fst upperBound + 1
                   rotated = take n $ drop (n - amt `mod` n) $ cycle es
               forM_  (zip [0 ..] rotated) $ \(i, val) -> do
                   writeArray grid (i, y) val
           RotateCol x amt -> do
               es <- forM [0..snd upperBound] $ \j -> readArray grid (x, j)
               let n = snd upperBound + 1
                   rotated = take n $ drop (n - amt `mod` n) $ cycle es
               forM_ (zip [0..] rotated) $ \(j, val) -> do
                   writeArray grid (x, j) val
   freeze grid

process :: String -> String
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right ops ->
            let finalGrid = applyOps ops
                lights = amap (\x -> if x == 1 then '*' else ' ') finalGrid
                ((minX, minY), (maxX, maxY)) = bounds finalGrid
            in unlines [ [ lights ! (x, y) | x <- [minX .. maxX] ]
                       | y <- [minY .. maxY]
                       ]

main :: IO ()
main = do
    args <- getArgs
    progname <- getProgName
    case args of
        [filename] -> do
            content <- readFile filename
            let result = process content
            putStrLn result
        _ -> usage progname
