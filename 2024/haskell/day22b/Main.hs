module Main ( main, changeSequence ) where

import Data.Bits ( xor )
import Data.List ( foldl', zip4 )
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )

usage :: IO ()
usage = do
    progname <- getProgName
    putStrLn $ "usage: " ++ progname ++ " <file>"
    exitFailure

mix :: Int -> Int -> Int
mix x y = x `xor` y

prune :: Int -> Int
prune x = x `mod` 16777216

step :: Int -> Int
step = step3 . step2 . step1
    where
        step1 x = prune $ mix x (x * 64)
        step2 x = prune $ mix x (x `div` 32)
        step3 x = prune $ mix x (x * 2048)

changeSequence :: Int -> Int -> [(Int, Int)]
changeSequence secret n =
    let (result, _, _) =
            foldl'
                (\(l, s, prev) _ ->
                    let s' = step s
                in (l ++ [(s' `mod` 10, (s' `mod` 10) - prev)], s', s' `mod` 10)
            )
            ([], secret, secret `mod` 10)
            [1..n]
    in result

encodeSequence :: (Int, Int, Int, Int) -> Int
encodeSequence (a, b, c, d) = ((a + 9) * 6859) + ((b + 9) * 361) + ((c + 9) * 19) + (d + 9)

process :: String -> Int
process contents =
    let secrets = map read (lines contents) :: [Int]
        seqPrices =
            foldl'
                (\acc secret ->
                    let changes = changeSequence secret 2000
                        s = map snd $ take 4 changes
                        seq4 = (s !! 0, s !! 1, s !! 2, s !! 3)
                        monkeySeq =
                            foldl'
                                (\acc' quad ->
                                    let (a, b, c, d) = quad
                                        seq4' = (snd a, snd b, snd c, snd d)
                                        seqKey = encodeSequence seq4'
                                    in if seqKey `Map.notMember` acc'
                                        then Map.insert seqKey (fst d) acc'
                                        else acc'
                                )
                                (Map.singleton (encodeSequence seq4) (fst (changes !! 3)))
                                (zip4 (drop 1 changes) (drop 2 changes) (drop 3 changes) (drop 4 changes))
                    in Map.unionWith (+) acc monkeySeq
                )
                Map.empty
                secrets
    in maximum (Map.elems seqPrices)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            contents <- readFile filename
            let result = process contents
            putStrLn $ "result = " ++ show result
        _ -> usage
