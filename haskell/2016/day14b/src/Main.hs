module Main ( main ) where

import Control.Monad.State.Strict
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as ByteString
import Data.ByteString.Builder (toLazyByteString, word8HexFixed)
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Char (ord)
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import Data.List (isInfixOf)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

type Cache = IntMap String
type HashM = State Cache

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <key>"
    exitFailure

toHexBS :: ByteString.ByteString -> ByteString.ByteString
toHexBS bs =
    LazyByteString.toStrict $ toLazyByteString $ mconcat (map word8HexFixed (ByteString.unpack bs))

computeHash :: String -> Int -> String
computeHash salt n =
    let input0 = ByteString.pack $ map (fromIntegral . ord) (salt ++ show n)
        go 0 h = h
        go k h = go (k-1) (MD5.hash (toHexBS h))
        finalDigest = go 2016 (MD5.hash input0)
    in map (toEnum . fromEnum) (ByteString.unpack (toHexBS finalDigest))

memoComputeHash :: String -> Int -> HashM String
memoComputeHash salt n = do
    cache <- get
    case IntMap.lookup n cache of
        Just h  -> return h
        Nothing -> do
            let h = computeHash salt n
            modify' (IntMap.insert n h)
            pure h

findThreeConsecutive :: String -> Maybe Char
findThreeConsecutive (a:b:c:rest)
    | a == b && b == c = Just a
    | otherwise        = findThreeConsecutive (b:c:rest)
findThreeConsecutive _ = Nothing

hasFiveConsecutive :: Char -> String -> Bool
hasFiveConsecutive ch str = replicate 5 ch `isInfixOf` str

findKeys :: String -> HashM [Int]
findKeys salt = go 0 []
  where
    go n acc
        | length acc >= 64 = pure (reverse acc)
        | otherwise =  do
            h <- memoComputeHash salt n
            case findThreeConsecutive h of
                Just ch -> do
                    next1000 <- mapM (memoComputeHash salt) [n+1..n+1000]
                    if any (hasFiveConsecutive ch) next1000
                        then go (n+1) (n:acc)
                        else go (n+1) acc
                Nothing -> go (n+1) acc

process :: String -> Int
process salt = evalState (findKeys salt) IntMap.empty !! 63

main :: IO ()
main = do
    args <- getArgs
    progname <- getProgName
    case args of
        [salt] -> do
            let result = process salt
            putStrLn $ "result = " ++ show result
        _ -> usage progname
