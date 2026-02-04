module Main (main) where

import Control.DeepSeq
import Data.Int (Int32)
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Clock
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Parsec
import Text.Parsec.String

type HappinessTable = Map.Map (String, String) Int

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

file :: Parser [(String, String, Int)]
file = line `endBy` newline

line :: Parser (String, String, Int)
line = do
       person1 <- many1 letter
       _ <- string " would "
       action <- string "gain" <|> string "lose"
       _ <- string " "
       amountTxt <- many1 digit
       _ <- string " happiness units by sitting next to "
       person2 <- many1 letter
       _ <- string "."
       let amount = case action of
                        "gain" -> read amountTxt
                        "lose" -> -(read amountTxt)
                        _ -> 0
       return (person1, person2, amount)

buildHapTable :: [(String, String, Int)] -> HappinessTable
buildHapTable = foldl' (\acc (p1, p2, h) -> Map.insert (p1, p2) h acc) Map.empty

computeScore :: HappinessTable -> [String] -> Int
computeScore hapTable people =
    sum [ hapTable Map.! (p, l) + hapTable Map.! (p, r) | (p, l, r) <- triples people ]
  where
    triples xs = zip3 xs (last xs : init xs) (tail xs ++ [head xs])

process :: String -> Int32
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right arrangements ->
            let hapTable = buildHapTable arrangements
                people = Set.toList $ Set.fromList [ p1 | (p1, _, _) <- arrangements ]
                (fixed:rest) = people
            in fromIntegral $ maximum $ map (computeScore hapTable . (fixed:)) (permutations rest)

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
