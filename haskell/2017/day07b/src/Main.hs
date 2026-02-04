module Main ( main ) where

import Data.Functor ( (<&>) )
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )
import System.IO ( hPutStrLn, stderr )
import Text.Parsec ( try, char, digit, letter, newline, spaces, string, between, many1, option, sepBy1, sepEndBy1, parse )
import Text.Parsec.String ( Parser )
import Control.DeepSeq
import System.Clock

data Program = Program
    { weight :: Int
    , name :: String
    , children :: [String]
    } deriving (Show)

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

file :: Parser (Map String Program)
file = (line `sepEndBy1` newline) <&> Map.fromList
  where
    line = do
        n <- many1 letter
        spaces
        w <- between (char '(') (char ')') (many1 digit)
        cs <- option [] (try (spaces >> string "->" >> spaces >> sepBy1 (many1 letter) (string ", ")))
        return (n, Program { weight = read w, name = n, children = cs })

findRoot :: Map String Program -> String
findRoot programs =
  case [n | n <- Map.keys programs
          , all (\p -> n `notElem` children p) (Map.elems programs)] of
    [root] -> root
    _      -> error "No unique root found"

computeWeights :: Map String Program -> String -> Map String Int
computeWeights programs = go Map.empty
  where
    go acc n
      | Map.member n acc = acc
      | otherwise =
          let p = programs Map.! n
              acc' = foldl go acc (children p)
              w = weight p + sum [ acc' Map.! c | c <- children p ]
          in Map.insert n w acc'

findImbalance :: Map String Program -> Map String Int -> String -> Maybe Int
findImbalance programs weights root =
  let p = programs Map.! root
      childWeights = [ (c, weights Map.! c) | c <- children p ]
      badChild [] = Nothing
      badChild ws =
        let groups = Map.fromListWith (++) [(w, [c]) | (c, w) <- ws]
        in if Map.size groups <= 1
             then Nothing
             else case Map.toList groups of
               [(badW, [badC]), (goodW, _)] -> adjust badC badW goodW
               [(goodW, _), (badW, [badC])] -> adjust badC badW goodW
               _ -> Nothing
      adjust badC badW goodW =
        let diff = goodW - badW
            newW = weight (programs Map.! badC) + diff
        in Just (badC, newW)
  in case badChild childWeights of
       Just (badC, correctW) ->
         case findImbalance programs weights badC of
           Just w  -> Just w
           Nothing -> Just correctW
       Nothing -> Nothing

process :: String -> Maybe Int
process content = case parse file "" content of
  Left err -> error (show err)
  Right programs ->
    let root = findRoot programs
        weights = computeWeights programs root
    in findImbalance programs weights root


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
            case result of
                Nothing -> putStrLn "No imbalance found"
                Just v  -> do
                    putStrLn $ "result = " ++ show v
            putStrLn $ "elapsed time: " ++ showTime elapsed
        _ -> usage progname
