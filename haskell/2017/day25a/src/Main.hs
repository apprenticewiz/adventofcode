module Main ( main ) where

import Control.DeepSeq
import Control.Monad
import Control.Monad.State
import Data.IntMap ( IntMap )
import Data.Map.Strict ( Map )
import qualified Data.IntMap as IntMap
import qualified Data.Map.Strict as Map
import System.Clock
import System.Environment
import System.Exit
import System.IO
import Text.Parsec hiding ( State )
import Text.Parsec.String

data Direction = L | R
                 deriving (Eq, Show)

data StateRules = StateRules
    { zero :: (Int, Direction, Char)
    , one  :: (Int, Direction, Char)
    }
    deriving (Eq, Show)

type Machine = Map Char StateRules

type Tape = IntMap Int

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

file :: Parser (Char, Int, Machine)
file = do
    _ <- string "Begin in state "
    initState <- letter
    _ <- string "." >> newline
    _ <- string "Perform a diagnostic checksum after "
    numSteps <- read <$> many1 digit
    _ <- string " steps." >> newline >> newline
    states <- Map.fromList <$> parseState `sepEndBy1` newline <* eof
    return (initState, numSteps, states)

txtToDir :: String -> Direction
txtToDir "left"  = L
txtToDir "right" = R
txtToDir _       = error "invalid direction"

parseState :: Parser (Char, StateRules)
parseState = do
    _ <- string "In state "
    name <- letter
    _ <- string ":" >> newline
    zeroRule <- parseRule "0"
    oneRule <- parseRule "1"
    return (name, StateRules { zero = zeroRule, one = oneRule })

parseRule :: String -> Parser (Int, Direction, Char)
parseRule currentVal = do
    _ <- string ("  If the current value is " ++ currentVal ++ ":") >> newline
    _ <- string "    - Write the value "
    n <- read <$> (string "0" <|> string "1")
    _ <- string "." >> newline
    _ <- string "    - Move one slot to the "
    dir <- txtToDir <$> (string "left" <|> string "right")
    _ <- string "." >> newline
    _ <- string "    - Continue with state "
    next <- letter
    _ <- string "." >> newline
    return (n, dir, next)

run :: Int -> Machine -> State (Int, Char, Tape) Int
run numSteps machine = do
    replicateM_ numSteps (step machine)
    (_, _, finalTape) <- get
    return (length $ filter (== 1) $ IntMap.elems finalTape)
  where
    step :: Machine -> State (Int, Char, Tape) ()
    step m = do
        (cursor, currentState, tape) <- get
        let currentVal = IntMap.findWithDefault 0 cursor tape
            rules = m Map.! currentState
            (val, dir, nextState) = if currentVal == 0 then zero rules else one rules
            tape' = IntMap.insert cursor val tape
            cursor' = case dir of
                L -> cursor - 1
                R -> cursor + 1
        put (cursor', nextState, tape')

process :: String -> Int
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right (initState, numSteps, machine) -> evalState (run numSteps machine) (0, initState, IntMap.empty)

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
