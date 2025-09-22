module Main ( main ) where

import Data.Char (ord)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <string>"
    exitFailure

inc :: String -> String
inc = reverse . go . reverse
  where
    go [] = "a"
    go (x:xs)
      | x == 'z'  = 'a' : go xs
      | otherwise = succ x : xs

prop1 :: String -> Bool
prop1 [] = False
prop1 [_] = False
prop1 [_, _] = False
prop1 (a:b:c:xs)
  | ord b == ord a + 1 && ord c == ord b + 1 = True
  | otherwise = prop1 (b:c:xs)

prop2 :: String -> Bool
prop2 = all (`notElem` "iol")

prop3 :: String -> Bool
prop3 s = length (pairs s) >= 2
  where
    pairs [] = []
    pairs [_] = []
    pairs (a:b:xs)
      | a == b    = a : pairs xs
      | otherwise = pairs (b:xs)

valid :: String -> Bool
valid s = prop1 s && prop2 s && prop3 s

process :: String -> String
process = head . filter valid . tail . iterate inc

main :: IO ()
main = do
    args <- getArgs
    progname <- getProgName
    case args of
        [inputStr] -> do
            let result = process inputStr
            putStrLn $ "result = " ++ result
        _ -> usage progname
