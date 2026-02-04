module Main ( main ) where

import Control.DeepSeq
import Data.List
import Data.Map.Strict ( Map )
import Data.Ord
import qualified Data.Map.Strict as Map
import System.Clock
import System.Environment
import System.Exit
import System.IO
import Text.Parsec

type Parser a = Parsec String Int a

data Team = Immune | Infection
            deriving (Eq, Show)

data Group = Group
  { team       :: Team
  , gid        :: Int
  , units      :: Int
  , hp         :: Int
  , weaknesses :: [String]
  , immunities :: [String]
  , attackDmg  :: Int
  , attackType :: String
  , initiative :: Int
  } deriving (Show)

file :: Parser [Group]
file = do
    _ <- string "Immune System:" >> newline
    immuneSystem <- groupParser Immune `endBy1` newline
    _ <- newline
    _ <- string "Infection:" >> newline
    infection <- groupParser Infection `endBy1` newline <* eof
    return (immuneSystem ++ infection)

groupParser :: Team -> Parser Group
groupParser t = do
    currentGid <- getState
    modifyState (+1)
    u <- integer
    _ <- string " units each with "
    h <- integer
    _ <- string " hit points "
    (weak, immune) <- option ([], []) modifiers
    _ <- string "with an attack that does "
    dmg <- integer
    _ <- space
    atype <- many1 letter
    _ <- string " damage at initiative "
    initValue <- integer
    return $ Group t currentGid u h weak immune dmg atype initValue

modifiers :: Parser ([String], [String])
modifiers = do
    _ <- char '('
    mods <- modifier `sepBy` string "; "
    _ <- string ") "
    let weak = concat [w | ("weak", w) <- mods]
    let immune = concat [i | ("immune", i) <- mods]
    return (weak, immune)

modifier :: Parser (String, [String])
modifier = do
    modType <- string "weak" <|> string "immune"
    _ <- string " to "
    types <- many1 letter `sepBy` string ", "
    return (modType, types)

integer :: Parser Int
integer = read <$> ((++) <$> option "" (string "-") <*> many1 digit)

effectivePower :: Group -> Int
effectivePower grp = units grp * attackDmg grp

damage :: Group -> Group -> Int
damage attacker defender
  | attackType attacker `elem` immunities defender = 0
  | attackType attacker `elem` weaknesses defender = 2 * effectivePower attacker
  | otherwise = effectivePower attacker

targetSelection :: [Group] -> Map Int Int
targetSelection groups = go sortedGroups Map.empty []
  where
    sortedGroups = sortBy (comparing (Down . effectivePower) <> comparing (Down . initiative)) groups

    go [] acc _ = acc
    go (g:gs) acc chosen =
        let enemies = filter (\e -> team e /= team g && gid e `notElem` chosen) groups
            best = pickTarget g enemies
        in case best of
            Just target -> go gs (Map.insert (gid g) (gid target) acc) (gid target : chosen)
            Nothing -> go gs acc chosen

pickTarget :: Group -> [Group] -> Maybe Group
pickTarget g enemies =
    case sortBy cmp candidates of
        ((dmg, _, _, target):_) | dmg > 0 -> Just target
        _ -> Nothing
  where
    candidates = [ (damage g e, effectivePower e, initiative e, e) | e <- enemies ]
    cmp = comparing (Down . (\(d, _, _, _) ->d))
        <> comparing (Down . (\(_, ep, _, _) -> ep))
        <> comparing (Down . (\(_, _, i, _) -> i))

attackingPhase :: [Group] -> Map Int Int -> ([Group], Bool)
attackingPhase groups targets = (remaining, anyKills)
  where
    order = sortBy (comparing (Down . initiative)) groups

    (finalGroups, anyKills) = foldl' applyAttack (groups, False) order

    applyAttack (gs, killedFlag) attacker =
        case find ((== gid attacker) . gid) gs of
            Just currentAttacker | units currentAttacker > 0 ->
                case Map.lookup (gid attacker) targets of
                    Just targetId ->
                        case find ((== targetId) . gid) gs of
                            Just target ->
                                let dmg = damage currentAttacker target
                                    killed = min (units target) (dmg `div` hp target)
                                    gs' = map (updateTarget targetId killed) gs
                                in (gs', killedFlag || killed > 0)
                            _ -> (gs, killedFlag)
                    _ -> (gs, killedFlag)
            _ -> (gs, killedFlag)

    updateTarget targetId killed grp
      | gid grp == targetId = grp { units = units grp - killed }
      | otherwise = grp

    remaining = filter ((> 0) . units) finalGroups

battle :: [Group] -> [Group]
battle = go
  where
    go gs
      | not (bothTeamsExist gs) = gs
      | otherwise =
            let targets = targetSelection gs
                (gs', anyKills) = attackingPhase gs targets
            in if not anyKills
                then gs
                else go gs'

    bothTeamsExist gs = any ((== Immune) . team) gs && any ((== Infection) . team) gs

process :: String -> Int
process content =
    case runParser file 0 "" content of
        Left err -> error (show err)
        Right groups -> sum . map units . battle $ groups

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
