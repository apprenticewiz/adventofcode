module Main ( main ) where

import Control.Monad.State
import Data.Bits
import Data.Char (isDigit)
import Data.Int (Int32)
import qualified Data.Map as Map
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

data Operation = AssignOp String
               | NotOp String
               | AndOp String String
               | OrOp String String
               | LeftShiftOp String Int
               | RightShiftOp String Int
               deriving (Show)

type Cache = Map.Map String Word
type EvalState = State Cache

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

evaluate :: Map.Map String Operation -> String -> EvalState Word
evaluate ops expr
    | all isDigit expr = pure (read expr)
    | otherwise = do
        cache <- get
        case Map.lookup expr cache of
            Just v -> pure v
            Nothing -> do
                let op = ops Map.! expr
                v <- case op of
                        AssignOp src          -> evaluate ops src
                        NotOp src             -> complement <$> evaluate ops src
                        AndOp src1 src2       -> (.&.) <$> evaluate ops src1 <*> evaluate ops src2
                        OrOp src1 src2        -> (.|.) <$> evaluate ops src1 <*> evaluate ops src2
                        LeftShiftOp src amt   -> (`shift` amt) <$> evaluate ops src
                        RightShiftOp src amt  -> (`shift` (-amt)) <$> evaluate ops src
                let masked = v .&. 0xffff
                modify (Map.insert expr masked)
                pure masked

parseLine :: String -> (String, Operation)
parseLine line = case words line of
    [src, "->", dest] -> (dest, AssignOp src)
    ["NOT", src, "->", dest] -> (dest, NotOp src)
    [src1, "AND", src2, "->", dest] -> (dest, AndOp src1 src2)
    [src1, "OR", src2, "->", dest] -> (dest, OrOp src1 src2)
    [src, "LSHIFT", amt, "->", dest] -> (dest, LeftShiftOp src (read amt))
    [src, "RSHIFT", amt, "->", dest] -> (dest, RightShiftOp src (read amt))
    _ -> error ("malformed input line: " ++ line)

process :: String -> Int32
process content =
    let ops = foldl (\acc line -> let (dest, op) = parseLine line in Map.insert dest op acc) Map.empty (lines content)
        a = evalState (evaluate ops "a") Map.empty
        newOps = Map.insert "b" (AssignOp (show a)) ops
        a' = evalState (evaluate newOps "a") Map.empty
    in fromIntegral a'

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
