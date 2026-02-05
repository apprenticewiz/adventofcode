module Main ( main ) where

import Control.DeepSeq
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Clock
import System.Environment
import System.Exit
import System.IO

data Orbit = Orbit
    { parent :: Maybe String
    , children :: [String]
    } deriving (Eq, Show)

process :: String -> Int
process content =
    let orbits = foldr insertOrbit Map.empty (lines content)
        youParent = getParent orbits "YOU"
        sanParent = getParent orbits "SAN"
    in bfs orbits youParent sanParent
  where
    insertOrbit :: String -> Map String Orbit -> Map String Orbit
    insertOrbit line orbits =
        case span (/= ')') line of
            (parentObj, _:childObj) ->
                let existingChild = Map.findWithDefault (Orbit Nothing []) childObj orbits
                    childOrbit = Orbit (Just parentObj) (children existingChild)
                    orbits' = Map.insert childObj childOrbit orbits
                    parentOrbit = Map.findWithDefault (Orbit Nothing []) parentObj orbits'
                    parentOrbit' = parentOrbit { children = childObj : children parentOrbit }
                    orbits'' = Map.insert parentObj parentOrbit' orbits'
                in orbits''
            _ -> error ("malformed input line: " ++ line)

    getParent :: Map String Orbit -> String -> String
    getParent orbits node =
        case Map.lookup node orbits of
            Just (Orbit (Just p) _) -> p
            _ -> error (node ++ " not found or has no parent")

    neighbors :: Map String Orbit -> String -> [String]
    neighbors orbits node =
        case Map.lookup node orbits of
            Just (Orbit mp cs) -> maybe cs (: cs) mp
            Nothing -> []

    bfs :: Map String Orbit -> String -> String -> Int
    bfs orbits start goal = go (Set.singleton start) [start] 0
      where
        go _ [] _ = error "no path found"
        go visited frontier dist
            | goal `elem` frontier = dist
            | otherwise =
                let nexts = concatMap (neighbors orbits) frontier
                    newNodes = filter (`Set.notMember` visited) nexts
                    visited' = foldl (flip Set.insert) visited newNodes
                in go visited' newNodes (dist + 1)

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
