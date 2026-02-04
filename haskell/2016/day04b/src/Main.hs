module Main ( main ) where

import Data.Char
import Data.List
import qualified Data.Map.Strict as Map
import Data.Ord
import System.Environment
import System.Exit
import System.IO
import Text.Parsec
import Text.Parsec.String
import Control.DeepSeq
import System.Clock

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

file :: Parser [([String], Int, String)]
file = (line `sepEndBy` newline) <* eof

line :: Parser ([String], Int, String)
line = do
       nameParts <- (many1 letter) `endBy1` (string "-")
       idNum <- many1 digit
       checksum <- (between (string "[") (string "]") (many1 letter))
       return (nameParts, read idNum, checksum)

isReal :: ([String], Int, String) -> Bool
isReal (nameParts, _, checksum) =
    let histo = genHisto nameParts
        myChecksum = calcChecksum histo
    in myChecksum == checksum
  where
    genHisto :: [String] -> Map.Map Char Int
    genHisto = foldr addString Map.empty

    addString :: String -> Map.Map Char Int -> Map.Map Char Int
    addString s acc = foldr (\ch acc' -> Map.insertWith (+) ch 1 acc') acc s

    calcChecksum :: Map.Map Char Int -> String
    calcChecksum h =
        let vs = reverse $ sortBy (comparing snd) (Map.assocs h)
            n1 = snd (vs !! 0)
            (as, vs') = partition (\(_, n) -> n == n1) vs
            n2 = snd (vs' !! 0)
            (bs, vs'') = partition (\(_, n) -> n == n2) vs'
            n3 = snd (vs'' !! 0)
            (cs, vs''') = partition (\(_, n) -> n == n3) vs''
            n4 = snd (vs''' !! 0)
            (ds, vs'''') = partition (\(_, n) -> n == n4) vs'''
            n5 = snd (vs'''' !! 0)
            (es, _) = partition (\(_, n) -> n == n5) vs''''
        in take 5 $ concat (map (sort . map fst) [as, bs, cs, ds, es])

findRoom :: [([String], Int, String)] -> Int
findRoom rooms = head [ i | (encrypted, i, _) <- rooms
                          , length encrypted == 3
                          , let decrypted = decrypt i encrypted
                          , decrypted == ["northpole", "object", "storage"]
                      ]
  where
    decrypt :: Int -> [String] -> [String]
    decrypt i =  map (decryptWord i)

    decryptWord :: Int -> String -> String
    decryptWord i s = map (\ch ->
                               let charset = map chr ([ord ch..ord 'z'] ++ [ord 'a'..(ord ch)-1])
                               in (cycle charset) !! i
                          ) s

process :: String -> Int
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right rooms ->
            let realRooms = filter isReal rooms
            in findRoom realRooms


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
