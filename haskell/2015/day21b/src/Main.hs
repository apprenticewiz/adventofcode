module Main (main) where

import Data.Int (Int32)
import Data.List
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Parsec hiding (State)
import Text.Parsec.String
import Control.DeepSeq
import System.Clock

data ItemType = Armor | Weapon | Ring
                deriving (Eq, Show)

data Item = Item { name   :: String
                 , kind   :: ItemType
                 , cost   :: Int
                 , damage :: Int
                 , armor  :: Int
                 }

data Player = Player { playerHP     :: Int
                     , playerDamage :: Int
                     , playerArmor  :: Int
                     }

data Outcome = Win | Die
               deriving (Eq, Show)

store :: [Item]
store = [ Item { name = "Dagger",     kind = Weapon, cost = 8,   damage = 4, armor = 0 }
        , Item { name = "Shortsword", kind = Weapon, cost = 10,  damage = 5, armor = 0 }
        , Item { name = "Warhammer",  kind = Weapon, cost = 25,  damage = 6, armor = 0 }
        , Item { name = "Longsword",  kind = Weapon, cost = 40,  damage = 7, armor = 0 }
        , Item { name = "Greataxe",   kind = Weapon, cost = 74,  damage = 8, armor = 0 }
        , Item { name = "Leather",    kind = Armor,  cost = 13,  damage = 0, armor = 1 }
        , Item { name = "Chainmail",  kind = Armor,  cost = 31,  damage = 0, armor = 2 }
        , Item { name = "Splintmail", kind = Armor,  cost = 53,  damage = 0, armor = 3 }
        , Item { name = "Bandedmail", kind = Armor,  cost = 75,  damage = 0, armor = 4 }
        , Item { name = "Platemail",  kind = Armor,  cost = 102, damage = 0, armor = 5 }
        , Item { name = "Damage +1",  kind = Ring,   cost = 25,  damage = 1, armor = 0 }
        , Item { name = "Damage +2",  kind = Ring,   cost = 50,  damage = 2, armor = 0 }
        , Item { name = "Damage +3",  kind = Ring,   cost = 100, damage = 3, armor = 0 }
        , Item { name = "Defense +1", kind = Ring,   cost = 20,  damage = 0, armor = 1 }
        , Item { name = "Defense +2", kind = Ring,   cost = 40,  damage = 0, armor = 2 }
        , Item { name = "Defense +3", kind = Ring,   cost = 80,  damage = 0, armor = 3 }
        ]

weapons, armors, rings :: [Item]
weapons = filter ((== Weapon) . kind) store
armors  = filter ((== Armor) . kind) store
rings   = filter ((== Ring) . kind) store

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

computePlayer :: [Item] -> Player
computePlayer items =
    Player { playerHP = 100
           , playerDamage = sum [ damage item | item <- items ]
           , playerArmor = sum [ armor item | item <- items ]
           }

battle :: Player -> Player -> Outcome
battle player boss =
    let playerTakesDamagePerTurn = max 1 (playerDamage boss - playerArmor player)
        bossTakesDamagePerTurn = max 1 (playerDamage player - playerArmor boss)
        turnsToKillPlayer = ceiling ((fromIntegral (playerHP player) :: Float) /
                                     (fromIntegral playerTakesDamagePerTurn :: Float))
        turnsToKillBoss = ceiling ((fromIntegral (playerHP boss) :: Float) /
                                   (fromIntegral bossTakesDamagePerTurn :: Float))
    in if turnsToKillPlayer < turnsToKillBoss 
           then Die
           else Win

file :: Parser Player
file = do
       hp <- string "Hit Points: " *> many1 digit <* newline
       d <- string "Damage: " *> many1 digit <* newline
       a <- string "Armor: " *> many1 digit <* optional newline <* eof
       return Player { playerHP = read hp, playerDamage = read d, playerArmor = read a }

process :: String -> Int32
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right boss -> fromIntegral $ maximum $ map loadoutCost $ winningLoadouts boss
  where
    loadouts :: [[Item]]
    loadouts = [ w : a ++ rs | w <- weapons,
                               a <- filter ((<= 1) . length) (subsequences armors),
                               rs <- filter ((<= 2) . length) (subsequences rings) ]

    winningLoadouts :: Player -> [[Item]]
    winningLoadouts boss = [ loadout | loadout <- loadouts,
                             let player = computePlayer loadout,
                             battle player boss == Die ]

    loadoutCost :: [Item] -> Int
    loadoutCost = sum . map cost


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
