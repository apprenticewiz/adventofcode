{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns  #-}

module Main (main) where

import Control.DeepSeq
import qualified Data.Map.Strict as Map
import System.Clock
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Parsec hiding (State)
import Text.Parsec.String

data Player = Player
  { playerHP   :: Int
  , playerMana :: Int
  } deriving (Eq, Show)

data Boss = Boss
  { bossHP     :: Int
  , bossDamage :: Int
  } deriving (Eq, Show)

data Effect
  = Damage Int
  | LifeSteal Int
  | Armor Int
  | Recharge Int
  deriving (Eq, Show)

data Spell = Spell
  { spellEffect   :: Effect
  , spellCost     :: Int
  , spellDuration :: Int
  } deriving (Eq, Show)

type SpellBook = Map.Map String Spell

spells :: SpellBook
spells = Map.fromList
  [ ("Magic Missile", Spell (Damage 4)    53  0)
  , ("Drain",         Spell (LifeSteal 2) 73  0)
  , ("Shield",        Spell (Armor 7)    113  6)
  , ("Poison",        Spell (Damage 3)   173  6)
  , ("Recharge",      Spell (Recharge 101) 229 5)
  ]

data GameState = GameState
  { player       :: Player
  , boss         :: Boss
  , activeSpells :: [(String, Int)]
  , manaSpent    :: Int
  } deriving (Eq, Show)

damageBoss :: Int -> GameState -> GameState
damageBoss n gs@GameState{ boss = b@Boss{..} } =
  gs { boss = b { bossHP = bossHP - n } }

damagePlayer :: Int -> GameState -> GameState
damagePlayer n gs@GameState{ player = p@Player{..} } =
  gs { player = p { playerHP = playerHP - n } }

healPlayer :: Int -> GameState -> GameState
healPlayer n gs@GameState{ player = p@Player{..} } =
  gs { player = p { playerHP = playerHP + n } }

useMana :: Int -> GameState -> GameState
useMana n gs@GameState{ player = p@Player{..}, manaSpent } =
  gs { player = p { playerMana = playerMana - n }
     , manaSpent = manaSpent + n
     }

castable :: GameState -> [String]
castable GameState{ player = Player{..}, activeSpells } =
  [ name
  | (name, Spell{..}) <- Map.toList spells
  , spellCost <= playerMana
  , spellDuration == 0 || maybe True (<= 1) (lookup name activeSpells)
  ]

applyEffects :: GameState -> GameState
applyEffects gs@GameState{ activeSpells } =
  let step g (name, _) =
        case spellEffect (spells Map.! name) of
          Damage n   -> damageBoss n g
          Recharge n -> g { player = (player g) { playerMana = playerMana (player g) + n } }
          _          -> g
      gs' = foldl' step gs activeSpells
      activeSpells' = [ (s, d-1) | (s,d) <- activeSpells, d > 1 ]
  in gs' { activeSpells = activeSpells' }

castSpell :: String -> GameState -> GameState
castSpell name gs =
  let Spell{..} = spells Map.! name
      gs' = useMana spellCost gs
  in case (spellEffect, spellDuration) of
       (Damage n,    0) -> damageBoss n gs'
       (LifeSteal n, 0) -> healPlayer n (damageBoss n gs')
       (_, d) | d > 0   -> gs' { activeSpells = (name, d) : activeSpells gs }
       _                -> error "impossible spell case"

playerArmor :: GameState -> Int
playerArmor GameState{ activeSpells } =
  sum [ n | (name, _) <- activeSpells
          , Armor n <- [spellEffect (spells Map.! name)] ]

bossAttack :: GameState -> GameState
bossAttack gs@GameState{ boss = Boss{..} } =
  let damage = max 1 (bossDamage - playerArmor gs)
  in damagePlayer damage gs

takeTurns :: String -> GameState -> GameState
takeTurns spell gs =
  let gs1 = damagePlayer 1 gs
  in if playerHP (player gs1) <= 0
         then gs1
         else let gs2 = applyEffects gs1
              in if bossHP (boss gs2) <= 0
                 then gs2
                 else let gs3 = castSpell spell gs2
                      in if bossHP (boss gs3) <= 0
                             then gs3
                             else let gs4 = applyEffects gs3
                                  in if bossHP (boss gs4) <= 0
                                         then gs4
                                         else bossAttack gs4

search :: Int -> GameState -> Int
search best gs
  | playerHP (player gs) <= 0 = best
  | bossHP (boss gs)   <= 0   = min best (manaSpent gs)
  | manaSpent gs >= best      = best
  | otherwise =
      foldl' search best [ takeTurns spell gs | spell <- castable gs ]

file :: Parser Boss
file = do
  hp <- string "Hit Points: " *> many1 digit <* newline
  dmg <- string "Damage: " *> many1 digit <* optional newline <* eof
  pure Boss { bossHP = read hp, bossDamage = read dmg }

usage :: String -> IO ()
usage progname = do
  hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
  exitFailure

process :: String -> Int
process content =
  case parse file "" content of
    Left err      -> error (show err)
    Right initBoss ->
      let initState = GameState
            { player = Player 50 500
            , boss = initBoss
            , activeSpells = []
            , manaSpent = 0
            }
      in search maxBound initState

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
      let result = (process content)
      result `deepseq` return ()
      end <- getTime Monotonic
      let elapsed = diffTimeSpec start end
      putStrLn $ "result = " ++ show result
      putStrLn $ "elapsed time: " ++ showTime elapsed
    _ -> usage progname

