module Main ( main ) where

import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )
import System.IO ( hPutStrLn, stderr )
import Text.Parsec ( digit, letter, newline, spaces, string, eof, many1, option, sepEndBy1, (<|>), choice, try, parse )
import Text.Parsec.String ( Parser )
import Control.DeepSeq
import System.Clock

type Cpu = Map String Int

type Condition = (String, String, Int)

type Operation = (String, String, Int)

type Instruction = (Condition, Operation)

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

file :: Parser [Instruction]
file = (line `sepEndBy1` newline) <* eof

line :: Parser Instruction
line = do
       op <- operation
       _ <- string " if "
       cond <- condition
       return (cond, op)

operation :: Parser Operation
operation = do
    reg <- many1 letter
    spaces
    op <- string "inc" <|> string "dec"
    spaces
    n <- integer
    return (op, reg, n)

condition :: Parser Condition
condition = do
    reg <- many1 letter
    spaces
    op <- choice [ try (string "<=")
                 , try (string "<")
                 , try (string "==")
                 , try (string "!=")
                 , try (string ">=")
                 , try (string ">")
                 ]
    spaces
    n <- integer
    return (op, reg, n)

integer :: Parser Int
integer = do
    s <- option "" (string "-")
    amt <- many1 digit
    return $ read (s ++ amt)

process :: String -> Int
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right insns ->
            let (_, highest) = foldl' handleInstruction (Map.empty, 0) insns
            in highest
    where
        handleInstruction :: (Cpu, Int) -> Instruction -> (Cpu, Int)
        handleInstruction (cpu, highest) (cond, op) =
            let (condOp, condReg, condAmt) = cond
                (changeOp, opReg, opAmt) = op
                x = Map.findWithDefault 0 condReg cpu
                n = Map.findWithDefault 0 opReg cpu
                newN = case changeOp of
                    "inc" -> n + opAmt
                    "dec" -> n - opAmt
                    _     -> n
                cpu' = case condOp of
                    "<"  -> if x < condAmt then Map.insert opReg newN cpu else cpu
                    "<=" -> if x <= condAmt then Map.insert opReg newN cpu else cpu
                    "==" -> if x == condAmt then Map.insert opReg newN cpu else cpu
                    "!=" -> if x /= condAmt then Map.insert opReg newN cpu else cpu
                    ">=" -> if x >= condAmt then Map.insert opReg newN cpu else cpu
                    ">"  -> if x > condAmt then Map.insert opReg newN cpu else cpu
                    _    -> cpu
                currentHighest = maximum (Map.elems cpu')
                highest' = max currentHighest highest
            in (cpu', highest')


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
