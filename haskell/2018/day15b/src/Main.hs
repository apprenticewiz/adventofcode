module Main ( main ) where

import Data.List
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set ( Set )
import qualified Data.Set as Set
import System.Environment
import System.Exit
import System.IO
import Control.DeepSeq
import System.Clock

type Position = (Int, Int)

data Team = Elf | Goblin
            deriving (Eq, Show)

data Unit = Unit
    { position :: Position
    , team :: Team
    , health :: Int
    , attackPower :: Int
    }
    deriving (Eq, Show)

type MapGrid = [String]

data Game = Game
    { gmap :: MapGrid
    , units :: [Unit]
    }

newGame :: MapGrid -> Int -> Game
newGame initial atkPower =
    let us =
            [ if c == 'G'
                then Unit (x, y) Goblin 200 3
                else Unit (x, y) Elf 200 atkPower
            | (y, row) <- zip [0..] initial
            , (x, c)  <- zip [0..] row
            , c `elem` "GE"
            ]
        clearedMap =
            map (map (\c -> if c `elem` "GE" then '.' else c)) initial
    in Game clearedMap us

isGoblin :: Unit -> Bool
isGoblin u = team u == Goblin

ux :: Unit -> Int
ux u = fst $ position u

uy :: Unit -> Int
uy u = snd $ position u

neighbors :: [(Int, Int)]
neighbors = [(0, -1), (-1, 0), (1, 0), (0, 1)]

isAdjacent :: Unit -> Unit -> Bool
isAdjacent a b =
    abs (ux a - ux b) + abs (uy a - uy b) == 1

isOpen :: Game -> Int -> Int -> Bool
isOpen g x y =
    (gmap g !! y !! x == '.') &&
    all (\u -> ux u /= x || uy u /= y) (units g)

runGame :: Game -> Bool -> Maybe Int
runGame g0 failOnElfDeath = loop 0 g0
  where
    loop rounds g =
        let us0 = sortBy readingOrder (units g)
        in stepUnits rounds g us0 0

    stepUnits rounds g us i
        | i == length us = loop (rounds + 1) g{ units = us }
        | otherwise =
            let u = us !! i
                aliveTargets = filter ((/= isGoblin u) . isGoblin) us
            in if null aliveTargets
                then Just (rounds * sum (map health us))
                else
                    let g1 = g{ units = us }
                        (uMoved, g2) =
                            if any (isAdjacent u) aliveTargets
                                then (u, g1)
                                else tryMove g1 u aliveTargets
                        us2 =
                            replaceUnit us u uMoved
                        adjTargets =
                            sortBy unitCombatOrder
                            [ t | t <- us2, isGoblin t /= isGoblin uMoved
                                , isAdjacent uMoved t
                            ]
                    in case adjTargets of
                        [] ->
                            stepUnits rounds g2 us2 (i + 1)
                        (t:_) ->
                            let t' = t{ health = health t - attackPower uMoved }
                            in if health t' > 0
                                then
                                    let us3 = replaceUnit us2 t t'
                                    in stepUnits rounds g2 us3 (i + 1)
                                else
                                    if failOnElfDeath && not (isGoblin t)
                                        then Nothing
                                        else
                                            let us3 = delete t us2
                                                i' = if indexOf t us2 < i then i - 1 else i
                                            in stepUnits rounds g2 us3 (i' + 1)

readingOrder :: Unit -> Unit -> Ordering
readingOrder a b =
    compare (uy a, ux a) (uy b, ux b)

unitCombatOrder :: Unit -> Unit -> Ordering
unitCombatOrder a b =
    compare (health a, uy a, ux a) (health b, uy b, ux b)

indexOf :: Eq a => a -> [a] -> Int
indexOf x xs = fromJust (elemIndex x xs)

replaceUnit :: [Unit] -> Unit -> Unit -> [Unit]
replaceUnit [] _ _ = []
replaceUnit (u:us) old new
    | u == old  = new : us
    | otherwise = u : replaceUnit us old new

tryMove :: Game -> Unit -> [Unit] -> (Unit, Game)
tryMove g u targets =
    let inRange =
            Set.fromList
            [ (tx + dx, ty + dy)
            | Unit (tx, ty) _ _ _ <- targets
            , (dx, dy) <- neighbors
            , isOpen g (tx + dx) (ty + dy)
            ]
        (prevMap, _) = bfs g (ux u, uy u)
        reachableTargets =
            [ (x, y, dist)
            | (x, y) <- Set.toList inRange
            , Just path <- [reconstruct prevMap (ux u, uy u) (x,y)]
            , let dist = length path
            ]
        
    in if null reachableTargets
        then (u, g)
        else
            let minDist = minimum [d | (_, _, d) <- reachableTargets]
                chosenTarget = head $ sortOn (\(x, y, _) -> (y, x))
                    [ (x, y, d) | (x, y, d) <- reachableTargets, d == minDist ]
                (tx, ty, _) = chosenTarget
                (prevFromTarget, _) = bfs g (tx, ty)
                validSteps =
                    [ (nx, ny)
                    | (dx, dy) <- neighbors
                    , let nx = ux u + dx
                    , let ny = uy u + dy
                    , isOpen g nx ny
                    , Just pathFromStep <- [reconstruct prevFromTarget (tx, ty) (nx, ny)]
                    , length pathFromStep + 1 == minDist
                    ]                
            in case validSteps of
                [] -> (u, g)
                ((nx, ny):_) ->
                    let u' = u{ position = (nx, ny) }
                        us' = replaceUnit (units g) u u'
                    in (u', g{ units = us' })

bfs :: Game -> (Int, Int) -> (Map (Int, Int) (Int, Int), Set (Int, Int))
bfs g start =
    go (Map.singleton start (-1,-1)) (Set.singleton start) [start]
  where
    go prev visited [] = (prev, visited)
    go prev visited ((x, y):qs) =
        let next =
                [ (nx, ny)
                | (dx, dy) <- neighbors
                , let nx = x + dx ; ny = y + dy
                , not ((nx, ny) `Set.member` visited)
                , isOpen g nx ny
                ]
            visited' = foldr Set.insert visited next
            prev' = foldr (\p -> Map.insert p (x, y)) prev next
        in go prev' visited' (qs ++ next)

reconstruct :: Map (Int, Int) (Int, Int) -> (Int, Int) -> (Int, Int) -> Maybe [(Int, Int)]
reconstruct prevMap start end =
    if Map.notMember end prevMap
        then Nothing
        else go [] end
  where
    go acc p
        | p == start = Just (reverse acc)
        | otherwise =
            case Map.lookup p prevMap of
                Just parent ->
                    if parent == (-1, -1)
                        then Nothing
                        else go (p:acc) parent
                Nothing -> Nothing

process :: String -> Int
process content =
    let mapLines = lines content
    in go 4 mapLines
  where
    go atkPower mapLines =
        case runGame (newGame mapLines atkPower) True of
            Just outcome -> outcome
            Nothing -> go (atkPower + 1) mapLines
 
usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input>"
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
