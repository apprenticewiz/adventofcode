module Main ( main ) where

import Data.Set ( Set )
import qualified Data.Set as Set
import System.Environment
import System.Exit
import System.IO
import Text.Parsec
import Text.Parsec.String

type Pots = Set Int

type Rule = (String, Bool)
type Rules = Set String

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

file :: Parser (Pots, Rules)
file = do
    _ <- string "initial state: "
    ps <- many1 (oneOf "#.")
    let initPots = Set.fromList [ i | (i, p) <- zip [0..] ps, p == '#' ]
    _ <- newline >> newline
    rs <- rule `sepEndBy` newline <* eof
    let rules = Set.fromList [ p | (p, True) <- rs ]
    return (initPots, rules)

rule :: Parser Rule
rule = do
    lhs <- count 5 (oneOf "#.")
    _ <- string " => "
    rhs <- oneOf "#."
    let producesPlant = rhs == '#'
    return (lhs, producesPlant)

simulate :: Int -> Rules -> Pots -> Pots
simulate 0 _ pots = pots
simulate n rules pots = simulate (n - 1) rules (step rules pots)
  where
    step :: Set String -> Pots -> Pots
    step rs ps =
        Set.fromList [ i | i <- [lo-2..hi+2]
                         , let pat = patternAt i
                         , pat `Set.member` rs ]
      where
        lo = if Set.null ps then 0 else Set.findMin ps
        hi = if Set.null ps then 0 else Set.findMax ps
        patternAt i = map (\d -> if (i + d) `Set.member` ps then '#' else '.') [-2..2]

process :: String -> Int
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right (initPots, rules) -> sum $ Set.toList (simulate 20 rules initPots)

main :: IO ()
main = do
    args <- getArgs
    progname <- getProgName
    case args of
        [filename] -> do
            content <- readFile filename
            let result = process content
            putStrLn $ "result = " ++ show result
        _ -> usage progname
