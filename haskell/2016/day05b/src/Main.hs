module Main ( main ) where

import Control.Monad
import Control.Monad.State
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as ByteString
import Data.Char (ord)
import Data.Word (Word8)
import Data.Maybe (catMaybes, isJust)
import Numeric (readHex, showHex)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Control.DeepSeq
import System.Clock

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <key>"
    exitFailure

genPassword :: Int -> String -> State (Int, [Maybe Char]) String
genPassword len base =
    do
    forM_ [1..len] $ \_ -> do
        (n, password) <- get
        put (findNext (n + 1) base password)
    (_, finalPassword) <- get
    return $ catMaybes finalPassword
  where
    findNext :: Int -> String -> [Maybe Char] -> (Int, [Maybe Char])
    findNext i s p =
        let keyBytes = ByteString.pack $ map (fromIntegral . ord) (s ++ show i)
            digestBytes = MD5.hash keyBytes
            digest = toHex $ ByteString.unpack digestBytes
        in case digest of
               ('0':'0':'0':'0':'0':posChar:ch:_) ->
                   case readHex [posChar] of
                       [(pos, "")] | pos < length p ->
                           if isJust (p !! pos)
                               then findNext (i + 1) s p
                               else
                                   let updated = take pos p ++ [Just ch] ++ drop (pos + 1) p
                                   in (i, updated)
                       _ -> findNext (i + 1) s p
               _ -> findNext (i + 1) s p

    toHex :: [Word8] -> String
    toHex = foldl (\acc x -> let hexValue = showHex x ""
                             in if length hexValue == 1
                                then acc ++ "0" ++ hexValue
                                else acc ++ hexValue
                  ) ""

process :: String -> String
process base = evalState (genPassword 8 base) (1, replicate 8 Nothing)


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
        [base] -> do
            start <- getTime Monotonic
            let result = process base
            result `deepseq` return ()
            end <- getTime Monotonic
            let elapsed = diffTimeSpec start end
            putStrLn $ "result = " ++ show result
            putStrLn $ "elapsed time: " ++ showTime elapsed
        _ -> usage progname
