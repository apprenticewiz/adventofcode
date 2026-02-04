module Main ( main ) where

import Control.DeepSeq
import Data.Ord
import Data.PQueue.Max ( MaxQueue )
import qualified Data.PQueue.Max as MaxQueue
import System.Clock
import System.Environment
import System.Exit
import System.IO
import Text.Parsec
import Text.Parsec.String

data Bot = Bot
  { bx :: Int
  , by :: Int
  , bz :: Int
  , br :: Int
  } deriving (Show)

data Cube = Cube
  { ccenter :: (Int, Int, Int)
  , csize   :: Int
  } deriving (Show)

type Priority = (Int, Int, Int)

data QItem = QItem Priority Cube
             deriving (Show)

instance Eq QItem where
    (QItem p1 _) == (QItem p2 _) = p1 == p2

instance Ord QItem where
    compare (QItem p1 _) (QItem p2 _) = compare p1 p2

file :: Parser [Bot]
file = botParser `sepEndBy` newline <* eof

botParser :: Parser Bot
botParser = do
    _ <- string "pos=<"
    xVal <- integer
    _ <- string ","
    yVal <- integer
    _ <- string ","
    zVal <- integer
    _ <- string ">, r="
    rVal <- integer
    pure (Bot xVal yVal zVal rVal)

integer :: Parser Int
integer = read <$> ((++) <$> option "" (string "-") <*> many1 digit)

manhattan :: (Int, Int, Int) -> (Int, Int, Int) -> Int
manhattan (x1, y1, z1) (x2, y2, z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

distToCube :: (Int, Int, Int) -> Cube -> Int
distToCube (x, y, z) (Cube (cx, cy, cz) size) =
    let half = size `div` 2
        nx = clamp (cx - half, cx + half) x
        ny = clamp (cy - half, cy + half) y
        nz = clamp (cz - half, cz + half) z
    in manhattan (x, y, z) (nx, ny, nz)

botIntersectsCube :: Bot -> Cube -> Bool
botIntersectsCube (Bot x y z r) cube = distToCube (x, y, z) cube <= r

subdivide :: Cube -> [Cube]
subdivide (Cube (x, y, z) size)
    | size <= 1 =
        [ Cube (x, y, z) 0 ]
    | otherwise =
        let half = size `div` 2
            q = max 1 (half `div` 2)
            offs = [(-q, -q, -q), (-q, -q, q), (-q, q, -q), (-q, q, q),
                    (q, -q,-q),  (q, -q, q), (q, q, -q), (q, q, q)]
        in [ Cube (x + dx, y + dy, z + dz) half | (dx, dy, dz) <- offs ]

makePriority :: Cube -> [Bot] -> Priority
makePriority cube bots =
    let bcount = scoreCube cube bots
        d = distToCube (0, 0, 0) cube
        s = csize cube
    in (bcount, negate d, negate s)

scoreCube :: Cube -> [Bot] -> Int
scoreCube cube bots = length (filter (`botIntersectsCube` cube) bots)

search :: MaxQueue QItem -> [Bot] -> (Int, (Int, Int, Int))
search pq bots =
    case MaxQueue.getMax pq of
        Nothing -> error "Empty queue?"
        Just (QItem prio cube) ->
            let pq' = MaxQueue.deleteMax pq
                (bcount, _, _) = prio
            in if csize cube == 0
                then (bcount, ccenter cube)
                else
                    let children = subdivide cube
                        pq'' = foldl' (\acc c -> MaxQueue.insert (QItem (makePriority c bots) c) acc) pq' children
                    in search pq'' bots

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

process :: String -> Int
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right bots ->
            let xs = map bx bots
                ys = map by bots
                zs = map bz bots
                maxCoord = maximum (map abs (xs ++ ys ++ zs)) + 1
                size = head (dropWhile (< maxCoord) (iterate (* 2) 1))
                initial = Cube (0,0,0) size
                initialP = makePriority initial bots
                pq0 = MaxQueue.insert (QItem initialP initial) MaxQueue.empty
                (_, bestPoint) = search pq0 bots
            in manhattan bestPoint (0, 0, 0)

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
