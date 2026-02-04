module Main ( main ) where

import Control.DeepSeq
import Data.List
import Data.Map.Strict (Map)
import Data.Maybe
import qualified Control.Applicative as App
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import System.Clock
import System.Environment
import System.Exit
import System.IO
import Text.Parsec hiding (State)
import Text.Parsec.String

data Object = Generator String | Microchip String
              deriving (Eq, Ord, Show)

type Floor = Int

data BuildingState = BuildingState
  { elevator :: !Floor
  , pairs    :: ![(Floor, Floor)]
  } deriving (Eq, Ord, Show)

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

file :: Parser BuildingState
file = do
    floors <- sequence
        [ floorLine 0 "first"
        , floorLine 1 "second"
        , floorLine 2 "third"
        , floorLine 3 "fourth"
        ]
    eof
    let combined = foldl' merge Map.empty floors
        merge = Map.unionWith combine
        combine (g1, c1) (g2, c2) = (g1 App.<|> g2, c1 App.<|> c2)
        elemPairs = [ (fromMaybe (error err) gf, fromMaybe (error err) cf)
                    | (el, (gf, cf)) <- Map.toList combined
                    , let err = "Missing generator or chip for " ++ el
                    ]
    pure $ BuildingState 0 (sort elemPairs)

floorLine :: Int -> String -> Parser (Map String (Maybe Floor, Maybe Floor))
floorLine n ordinal = do
    _ <- string ("The " ++ ordinal ++ " floor contains ")
    objs <- objsParser
    optional newline
    pure (Map.fromListWith merge [toEntry o | o <- objs])
    where
        merge (g1, c1) (g2, c2) = (g1 App.<|> g2, c1 App.<|> c2)
        toEntry (Generator e) = (e, (Just n, Nothing))
        toEntry (Microchip e) = (e, (Nothing, Just n))

objsParser :: Parser [Object]
objsParser = (string "nothing relevant." >> pure []) <|> (objectParser `sepBy1` separator <* char '.')

separator :: Parser String
separator = try (string ", and ") <|> try (string " and ") <|> string ", "

objectParser :: Parser Object
objectParser = try generatorParser <|> microchipParser

generatorParser :: Parser Object
generatorParser = do
    element <- string "a " *> many1 letter
    _ <- string " generator"
    pure (Generator element)

microchipParser :: Parser Object
microchipParser = do
    element <- string "a " *> many1 letter
    _ <- string "-compatible microchip"
    pure (Microchip element)

canonical :: BuildingState -> BuildingState
canonical (BuildingState e ps) = BuildingState e (sort ps)

isValid :: BuildingState -> Bool
isValid (BuildingState _ ps) =
    all chipSafe ps
    where
        gensOnFloor f = [g | (g, _) <- ps, g == f]
        chipSafe (g, c)
            | g == c = True
            | otherwise = null (gensOnFloor c)

nextStates :: BuildingState -> [BuildingState]
nextStates (BuildingState e ps) =
    [ canonical (BuildingState e' newPairs)
    | e' <- [e - 1, e + 1]
    , e' >= 0, e < 4
    , moved <- filter (\x -> let n = length x in n >= 1 && n <= 2)
                      (subsequences (itemsOnFloor e))
    , let newPairs = moveItems e e' moved ps
    , isValid (BuildingState e' newPairs)
    ]
    where
        itemsOnFloor :: Floor -> [(Int, String)]
        itemsOnFloor f =
            [ (i, "G") | (i, (g, _)) <- zip [0..] ps, g == f ] ++
            [ (i, "C") | (i, (_, c)) <- zip [0..] ps, c == f ]

        moveItems :: Floor -> Floor -> [(Int, String)] -> [(Floor, Floor)] -> [(Floor, Floor)]
        moveItems from to moved ps' =
            [ case lookup i moved of
                  Just "G" -> (if g == from then to else g, c)
                  Just "C" -> (g, if c == from then to else c)
                  _        -> (g, c)
            | (i, (g, c)) <- zip [0..] ps'
            ]

bfs :: BuildingState -> Int
bfs start = go Set.empty (Seq.singleton (start, 0))
    where
        go _ Seq.Empty = error "No solution"
        go seen ((s, d) Seq.:<| q)
            | isGoal s = d
            | s `Set.member` seen = go seen q
            | otherwise =
                let nexts = nextStates s
                in go (Set.insert s seen) (q Seq.>< Seq.fromList [(n, d + 1) | n <- nexts])

isGoal :: BuildingState -> Bool
isGoal (BuildingState e ps) = e == 3 && all (\(g, c) -> g == 3 && c == 3) ps

process :: String -> Int
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right initSt -> bfs initSt

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
