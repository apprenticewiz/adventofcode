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

solve :: Int -> Rules -> Pots -> Int
solve target rules initPots = go 0 initPots Nothing
  where
    go :: Int -> Pots -> Maybe (Pots, Int) -> Int
    go gen pots prevState
      | gen == target = sum $ Set.toList pots
      | otherwise =
        let pots' = step rules pots
            norm = normalize pots'
            s' = sum $ Set.toList pots'
            gen' = gen + 1
        in case prevState of
            Just (prevNorm, prevSum)
              | norm == prevNorm ->
                let sumDelta = s' - prevSum
                    remaining = target - gen'
                in s' + remaining * sumDelta
            _ -> go gen' pots' (Just (norm, s'))

    step :: Rules -> Pots -> Pots
    step rs ps =
        Set.fromList [ i | i <- [lo-2..hi+2]
                            , let pat = patternAt i
                            , pat `Set.member` rs ]
        where
            lo = if Set.null ps then 0 else Set.findMin ps
            hi = if Set.null ps then 0 else Set.findMax ps
            patternAt i = map (\d -> if (i + d) `Set.member` ps then '#' else '.') [-2..2]

    normalize :: Pots -> Pots
    normalize ps =
        let lo = Set.findMin ps
        in Set.map (subtract lo) ps

process :: String -> Int
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right (initPots, rules) -> solve 50000000000 rules initPots

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
