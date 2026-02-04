module Main ( main ) where

import Control.DeepSeq
import Control.Monad.State
import Data.List
import Data.Map.Strict (Map)
import Data.Maybe
import qualified Data.Map.Strict as Map
import System.Clock
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

simulate :: Sim (Maybe BotId)
simulate = loop
  where
    loop = do
           s <- get
           let ready = [b | (b, cs) <- Map.toList (bots s), length cs == 2]
           case ready of
               [] -> pure Nothing
               (b:_) -> do
                        mChips <- takeChips b
                        case mChips of
                            Just (lo, hi) -> do
                                             if (lo, hi) == (17, 61)
                                                 then pure (Just b)
                                                 else do
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
