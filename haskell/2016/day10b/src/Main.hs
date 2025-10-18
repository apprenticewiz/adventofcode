module Main ( main ) where

import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.List
import Data.Map.Strict (Map)
import Data.Maybe
import System.Environment
import System.Exit
import System.IO
import Text.Parsec hiding (State)
import Text.Parsec.String

type BotId = Int

type OutputId = Int

data Target = Bot BotId | Output OutputId
              deriving (Eq, Show)

data Rule = Rule { lowTarget :: Target, highTarget :: Target }
            deriving (Eq, Show)

data StateData = StateData
    { bots    :: Map BotId [Int]
    , outputs :: Map OutputId [Int]
    , rules   :: Map BotId Rule
    }
    deriving (Show)

type Sim = State StateData

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

file :: Parser (Map BotId [Int], Map BotId Rule)
file = do
       linesParsed <- (line `sepEndBy` newline) <* eof
       return $ foldr (\l (bs, rs) ->
                         case l of
                             Left (botId, v) -> (Map.insertWith (++) botId [v] bs, rs)
                             Right (botId, r) -> (bs, Map.insert botId r rs)
                      ) (Map.empty, Map.empty) linesParsed

line :: Parser (Either (BotId, Int) (BotId, Rule))
line = try valueLine <|> botRuleLine

valueLine :: Parser (Either (BotId, Int) (BotId, Rule))
valueLine = do
            _ <- string "value "
            v <- read <$> many1 digit
            _ <- string " goes to bot "
            b <- read <$> many1 digit
            return $ Left (b, v)

botRuleLine :: Parser (Either (BotId, Int) (BotId, Rule))
botRuleLine = do
              _ <- string "bot "
              b <- read <$> many1 digit
              _ <- string " gives low to "
              lowTgt <- target
              _ <- string " and high to "
              highTgt <- target
              return $ Right (b, Rule lowTgt highTgt)

target :: Parser Target
target = do
         choice [ string "bot " >> (Bot . read <$> many1 digit)
                , string "output " >> (Output . read <$> many1 digit)
                ]

give :: Int -> Target -> Sim ()
give v (Bot b) =
    modify $ \s -> s { bots = Map.insertWith (++) b [v] (bots s) }
give v (Output o) =
    modify $ \s -> s { outputs = Map.insertWith (++) o [v] (outputs s) }

takeChips :: BotId -> Sim (Maybe (Int, Int))
takeChips b = do
              s <- get
              case Map.lookup b (bots s) of
                  Just [x, y] ->
                      let [lo, hi] = sort [x, y]
                      in do modify $ \s' -> s' { bots = Map.insert b [] (bots s') }
                            pure (Just (lo, hi))
                  _ -> pure Nothing

simulate :: Sim (Maybe Int)
simulate = loop
  where
    loop = do
           s <- get
           let os = outputs s
           case (Map.lookup 0 os >>= listToMaybe,
                 Map.lookup 1 os >>= listToMaybe,
                 Map.lookup 2 os >>= listToMaybe) of
               (Just a, Just b, Just c) -> pure (Just (a * b * c))
               _ -> do
                    let ready = [b | (b, cs) <- Map.toList (bots s), length cs == 2]
                    case ready of
                        [] -> pure Nothing
                        (b:_) -> do
                                 mChips <- takeChips b
                                 case mChips of
                                     Just (lo, hi) -> do
                                                      let Rule l h = (rules s) Map.! b
                                                      give lo l
                                                      give hi h
                                                      loop
                                     Nothing -> loop

process :: String -> Int
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right (initBots, initRules) ->
            let initState = StateData initBots Map.empty initRules
            in fromJust $ evalState simulate initState

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
