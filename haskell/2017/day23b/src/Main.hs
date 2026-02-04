{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.DeepSeq
import Control.Lens
import Control.Monad.State
import Data.Array
import System.Clock
import System.Environment
import System.Exit
import System.IO
import Text.Parsec hiding (State)
import Text.Parsec.String

data Operand
    = Register Char
    | Immediate Int
    deriving (Eq, Show)

data Instruction
    = Set Operand Operand
    | Sub Operand Operand
    | Mul Operand Operand
    | Jnz Operand Operand
    | Jnp Operand Operand
    deriving (Eq, Show)

type Program = Array Int Instruction

data Cpu = Cpu
    { _a :: Int
    , _b :: Int
    , _c :: Int
    , _d :: Int
    , _e :: Int
    , _f :: Int
    , _g :: Int
    , _h :: Int
    , _ip :: Int
    }
    deriving (Eq, Show)
makeLenses ''Cpu

file :: Parser Program
file = do
    insns <- instruction `sepEndBy` endOfLine
    eof
    return (listArray (0, length insns - 1) insns)

instruction :: Parser Instruction
instruction = do
    inst <- choice (map (try . string) ["set","sub","mul","jnz"])
    _ <- spaces
    op1 <- operand
    _ <- spaces
    op2 <- operand
    pure $ case inst of
        "set" -> Set op1 op2
        "sub" -> Sub op1 op2
        "mul" -> Mul op1 op2
        "jnz" -> Jnz op1 op2
        _     -> error "unreachable"

operand :: Parser Operand
operand = regOp <|> immOp

regOp :: Parser Operand
regOp = Register <$> letter

immOp :: Parser Operand
immOp = Immediate . read <$> ((++) <$> option "" (string "-") <*> many1 digit)

getReg :: Char -> Cpu -> Int
getReg reg cpu =
    case reg of
        'a' -> cpu ^. a
        'b' -> cpu ^. b
        'c' -> cpu ^. c
        'd' -> cpu ^. d
        'e' -> cpu ^. e
        'f' -> cpu ^. f
        'g' -> cpu ^. g
        'h' -> cpu ^. h
        _   -> error ("invalid register: " ++ [reg])

setReg :: Char -> Int -> Cpu -> Cpu
setReg reg val cpu =
    case reg of
        'a' -> cpu & a .~ val
        'b' -> cpu & b .~ val
        'c' -> cpu & c .~ val
        'd' -> cpu & d .~ val
        'e' -> cpu & e .~ val
        'f' -> cpu & f .~ val
        'g' -> cpu & g .~ val
        'h' -> cpu & h .~ val
        _   -> error ("invalid register: " ++ [reg])

resolve :: Operand -> Cpu -> Int
resolve (Immediate n) _ = n
resolve (Register r) cpu  = getReg r cpu

isPrime :: Int -> Bool
isPrime n
  | n <= 1    = False
  | n == 2    = True
  | even n    = False
  | otherwise = all (\x -> n `mod` x /= 0) [3, 5 .. floor (sqrt (fromIntegral n :: Double))]

runProgram :: Program -> State Cpu Int
runProgram prog = do
    let (l, u) = bounds prog
    cpu <- get
    if cpu ^. ip < l || cpu ^. ip > u
        then return (cpu ^. h)
        else case prog ! (cpu ^. ip) of
            Set (Register x) y -> do
                let yVal = resolve y cpu
                modify (setReg x yVal)
                modify (ip %~ (+ 1))
                runProgram prog
            Sub (Register x) y -> do
                let yVal = resolve y cpu
                let n = getReg x cpu
                modify (setReg x (n - yVal))
                modify (ip %~ (+ 1))
                runProgram prog
            Mul (Register x) y -> do
                let yVal = resolve y cpu
                let n = getReg x cpu
                modify (setReg x (n * yVal))
                modify (ip %~ (+ 1))
                runProgram prog
            Jnz x y -> do
                let xVal = resolve x cpu
                    yVal = resolve y cpu
                if xVal /= 0
                    then do
                        modify (ip %~ (+ yVal))
                        runProgram prog
                    else do
                        modify (ip %~ (+ 1))
                        runProgram prog
            Jnp x y -> do
                let xVal = resolve x cpu
                    yVal = resolve y cpu
                if not (isPrime xVal)
                    then do
                        modify (ip %~ (+ yVal))
                        runProgram prog
                    else do
                        modify (ip %~ (+ 1))
                        runProgram prog
            _ -> error ("invalid instruction: " ++ show (prog ! (cpu ^. ip)))

process :: String -> Int
process content =
    case parse file "" content of
        Left err  -> error (show err)
        Right prog -> evalState (runProgram (optimize prog)) initCpu
  where
    initCpu = Cpu 1 0 0 0 0 0 0 0 0
    optimize p = p // [(8, Jnp (Register 'b') (Immediate 17)), (9, Jnz (Immediate 1) (Immediate 17))]

usage :: String -> IO a
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

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
