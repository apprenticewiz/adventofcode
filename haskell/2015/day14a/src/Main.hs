module Main (main) where

import Control.Monad
import Control.Monad.State
import Data.Int (Int32)
import qualified Data.Map.Strict as Map
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Parsec hiding (State)
import Text.Parsec.String
import Control.DeepSeq
import System.Clock

data ReindeerAction = Flying Int
                    | Resting Int
                    deriving (Eq, Show)

type ReindeerState = (Int, Int, Int, Int, ReindeerAction)

type Race = Map.Map String ReindeerState

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

file :: Parser [(String, Int, Int, Int)]
file = line `endBy` newline

line :: Parser (String, Int, Int, Int)
line = do
       reindeer <- many1 letter
       _ <- string " can fly "
       velo <- many1 digit
       _ <- string " km/s for "
       flying <- many1 digit
       _ <- string " seconds, but then must rest for "
       resting <- many1 digit
       _ <- string " seconds."
       return (reindeer, read velo, read flying, read resting)

initRace :: [(String, Int, Int, Int)] -> Race
initRace entries =
    Map.fromList [(name, (0, velo, flying, resting, Flying flying)) | (name, velo, flying, resting) <- entries]

runStep :: State Race ()
runStep = do
          current <- get
          let next = Map.map tick current
          put next
  where
    tick :: ReindeerState -> ReindeerState
    tick (pos, velo, flying, resting, Flying timeLeft)
      | timeLeft == 0 = (pos, velo, flying, resting, Resting (resting - 1))
      | otherwise     = (pos + velo, velo, flying, resting, Flying (timeLeft - 1))
    tick (pos, velo, flying, resting, Resting timeLeft)
      | timeLeft == 0 = (pos + velo, velo, flying, resting, Flying (flying - 1))
      | otherwise     = (pos, velo, flying, resting, Resting (timeLeft - 1))

simulate :: Int -> Race -> Race
simulate n race = execState (replicateM_ n runStep) race

process :: String -> Int32
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right entries ->
            let race = initRace entries
                final = simulate 2503 race
            in fromIntegral $ maximum [pos | (_, (pos, _, _, _, _)) <- Map.toList final]


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
