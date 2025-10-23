module Main ( main ) where

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as ByteString
import qualified Data.Sequence as Seq
import System.Environment
import System.Exit
import System.IO
import Text.Printf

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

bfs :: String -> Maybe Path
bfs passcode = go (Seq.singleton (startPos, ""))
  where
    go Seq.Empty = Nothing
    go ((pos, path) Seq.:<| queue)
        | pos == endPos = Just path
        | otherwise =
            let doors = doorStates passcode path
                directions = "UDLR"
                nextSteps = [ (newPos, path ++ [dir])
                            | (isOpen, dir) <- zip doors directions
                            , isOpen
                            , Just newPos <- [move pos dir]
                            ]
                newQueue = queue Seq.>< Seq.fromList nextSteps
            in go newQueue

process :: String -> String  
process passcode =
    case bfs passcode of
        Just path -> path
        Nothing   -> error "No path found"

main :: IO ()
main = do
    args <- getArgs
    progname <- getProgName
    case args of
        [passcode] -> do
            let result = process passcode
            putStrLn $ "result = " ++ result
        _ -> usage progname
