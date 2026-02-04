module Main ( main ) where

import Control.DeepSeq
import Data.Char (ord)
import Data.List (isInfixOf)
import Data.Word (Word8)
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as ByteString
import System.Clock
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <key>"
    exitFailure

computeHash :: String -> Int -> String
computeHash salt n =
    let input = salt ++ show n
        digest = MD5.hash $ ByteString.pack $ map (fromIntegral . ord) input
    in concatMap (printf "%02x" :: Word8 -> String) (ByteString.unpack digest)

findThreeConsecutive :: String -> Maybe Char
findThreeConsecutive (a:b:c:rest)
    | a == b && b == c = Just a
    | otherwise        = findThreeConsecutive (b:c:rest)
findThreeConsecutive _ = Nothing

hasFiveConsecutive :: Char -> String -> Bool
hasFiveConsecutive ch str =
    let pattern = replicate 5 ch
    in pattern `isInfixOf` str

hashes :: String -> [String]
hashes salt = map (computeHash salt) [0..]

findKeys :: String -> [Int]
findKeys salt = go 0 (hashes salt)
  where
    go n (h:hs) =
        case findThreeConsecutive h of
            Just ch ->
                let next1000 = take 1000 hs
                in if any (hasFiveConsecutive ch) next1000
                      then n : go (n+1) hs
                      else go (n+1) hs
            Nothing -> go (n+1) hs
    go _ [] = []

process :: String -> Int
process salt =
    let keys = findKeys salt
    in keys !! 63

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
        [salt] -> do
            start <- getTime Monotonic
            let result = process salt
            result `deepseq` return ()
            end <- getTime Monotonic
            let elapsed = diffTimeSpec start end
            putStrLn $ "result = " ++ show result
            putStrLn $ "elapsed time: " ++ showTime elapsed
        _ -> usage progname
