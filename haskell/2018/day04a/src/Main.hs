module Main ( main ) where

import Data.List
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import System.Environment
import System.Exit
import System.IO
import Text.Parsec
import Text.Parsec.String
import Control.DeepSeq
import System.Clock

data Event = BeginShift Int
           | Sleeps Int
           | Awakens Int
           deriving (Eq, Show)

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

file :: Parser [Event]
file = line `sepEndBy1` newline <* eof

line :: Parser Event
line = do
    (_, t) <- between (char '[') (char ']') parseTS
    _ <- spaces
    choice [ shiftBegin, fallsAsleep t, wakesUp t ]

parseTS :: Parser ((Int, Int, Int), (Int, Int))
parseTS = do
    y <- read <$> many1 digit
    _ <- char '-'
    m <- read <$> many1 digit
    _ <- char '-'
    d <- read <$> many1 digit
    _ <- spaces
    h <- read <$> many1 digit
    _ <- char ':'
    mm <- read <$> many1 digit
    return ((y, m, d), (h, mm))

shiftBegin :: Parser Event
shiftBegin = do
    _ <- string "Guard #"
    n <- read <$> many1 digit
    _ <- string " begins shift"
    return (BeginShift n)

fallsAsleep :: (Int, Int) -> Parser Event
fallsAsleep (_, mm) = do
    _ <- string "falls asleep"
    return (Sleeps mm)

wakesUp :: (Int, Int) -> Parser Event
wakesUp (_, mm) = do
    _ <- string "wakes up"
    return (Awakens mm)

process :: String -> Int
process content =
    let sortedContent = (unlines . sort . lines) content
    in case parse file "" sortedContent of
            Left err -> error (show err)
            Right events ->
                let (sleepTable, _, _) = foldl' step (Map.empty, -1, -1) events
                    totalSleepTable = Map.mapWithKey (\_ gt -> sum (Map.elems gt)) sleepTable
                    (guard, _) = maximumBy (\(_, mm1) (_, mm2) -> compare mm1 mm2) (Map.toList totalSleepTable)
                    (maxMinute, _) = maximumBy (\(_, c1) (_, c2) -> compare c1 c2) (Map.toList (sleepTable Map.! guard))
                in guard * maxMinute
  where
    step :: (Map Int (Map Int Int), Int, Int) -> Event -> (Map Int (Map Int Int), Int, Int)
    step (sleepTable, currentGuard, sleepStart) event =
        case event of
            BeginShift n -> (sleepTable, n, sleepStart)
            Sleeps mm -> (sleepTable, currentGuard, mm)
            Awakens mm ->
                let guardTable = Map.findWithDefault Map.empty currentGuard sleepTable
                    guardTable' = Map.unionWith (+) guardTable (Map.fromList [(n, 1) | n <- [sleepStart..mm - 1]])
                in (Map.insert currentGuard guardTable' sleepTable, currentGuard, -1)


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
