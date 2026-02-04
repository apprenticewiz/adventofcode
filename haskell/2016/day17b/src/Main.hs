module Main ( main ) where

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as ByteString
import System.Environment
import System.Exit
import System.IO
import Text.Printf
import Control.DeepSeq
import System.Clock

type Coordinate = (Int, Int)
type Path = String

startPos :: Coordinate
startPos = (0, 0)

endPos :: Coordinate
endPos = (3, 3)

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <passcode>"
    exitFailure

doorStates :: String -> Path -> [Bool]
doorStates passcode path =
    let hashInput = passcode ++ path
        hash = MD5.hash $ ByteString.pack $ map (fromIntegral . fromEnum) hashInput
        hexChars = take 4 $ concatMap (printf "%02x") (ByteString.unpack hash)
        isOpen c = c `elem` "bcdef"
    in map isOpen hexChars

move :: Coordinate -> Char -> Maybe Coordinate
move (x, y) dir = case dir of
    'U' | y > 0 -> Just (x, y - 1)
    'D' | y < 3 -> Just (x, y + 1)
    'L' | x > 0 -> Just (x - 1, y)
    'R' | x < 3 -> Just (x + 1, y)
    _            -> Nothing

dfs :: String -> Int
dfs passcode = go startPos ""
    where
        go pos path
            | pos == endPos = length path
            | otherwise =
                let doors = doorStates passcode path
                    directions = "UDLR"
                    nextSteps = [ (newPos, path ++ [dir])
                                | (isOpen, dir) <- zip doors directions
                                , isOpen
                                , Just newPos <- [move pos dir]
                                ]
                in maximum $ 0 : [ go newPos newPath | (newPos, newPath) <- nextSteps ]

process :: String -> Int
process = dfs


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
        [passcode] -> do
            start <- getTime Monotonic
            let result = process passcode
            result `deepseq` return ()
            end <- getTime Monotonic
            let elapsed = diffTimeSpec start end
            putStrLn $ "result = " ++ show result
            putStrLn $ "elapsed time: " ++ showTime elapsed
        _ -> usage progname
