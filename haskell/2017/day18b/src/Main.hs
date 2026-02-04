module Main (main) where

import Control.DeepSeq
import Data.Array
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
    = Send Operand
    | Set Operand Operand
    | Add Operand Operand
    | Multiply Operand Operand
    | Modulo Operand Operand
    | Receive Operand
    | JumpGZ Operand Operand
    deriving (Eq, Show)

type Program = Array Int Instruction

data Cpu = Cpu
    { registers :: Map Char Int
    , ip        :: Int
    , queue     :: [Int]
    , waiting   :: Bool
    , sends     :: Int
    }
    deriving (Eq, Show)

file :: Parser Program
file = do
    insns <- instruction `sepEndBy` newline
    eof
    return (listArray (0, length insns - 1) insns)

instruction :: Parser Instruction
instruction = try unaryOp <|> binaryOp

unaryOp :: Parser Instruction
unaryOp = do
    inst <- string "snd" <|> string "rcv"
    _ <- spaces
    op <- operand
    pure $ case inst of
        "snd" -> Send op
        "rcv" -> Receive op
        _     -> error "unreachable"

binaryOp :: Parser Instruction
binaryOp = do
    inst <- choice (map (try . string) ["set","add","mul","mod","jgz"])
    _ <- spaces
    op1 <- operand
    _ <- spaces
    op2 <- operand
    pure $ case inst of
        "set" -> Set op1 op2
        "add" -> Add op1 op2
        "mul" -> Multiply op1 op2
        "mod" -> Modulo op1 op2
        "jgz" -> JumpGZ op1 op2
        _     -> error "unreachable"

operand :: Parser Operand
operand = regOp <|> immOp

regOp :: Parser Operand
regOp = Register <$> letter

immOp :: Parser Operand
immOp = Immediate . read <$> ((++) <$> option "" (string "-") <*> many1 digit)

getReg :: Char -> Map Char Int -> Int
getReg = Map.findWithDefault 0

setReg :: Char -> Int -> Map Char Int -> Map Char Int
setReg = Map.insert

resolve :: Operand -> Map Char Int -> Int
resolve (Immediate n) _ = n
resolve (Register r) m  = getReg r m

step :: Program -> (Cpu, Cpu) -> (Cpu, Cpu)
step prog (cpuA, cpuB)
    | ip cpuA < 0 || ip cpuA > snd (bounds prog) = (cpuA { waiting = True }, cpuB)
    | otherwise =
        case prog ! ip cpuA of
            Send op ->
                let val = resolve op (registers cpuA)
                    cpuA' = cpuA { ip = ip cpuA + 1, sends = sends cpuA + 1 }
                    cpuB' = cpuB { queue = queue cpuB ++ [val] }
                in (cpuA', cpuB')
            Set (Register x) op ->
                let val = resolve op (registers cpuA)
                in (cpuA { registers = setReg x val (registers cpuA), ip = ip cpuA + 1}, cpuB)
            Add (Register x) op ->
                let m = getReg x (registers cpuA)
                    n = resolve op (registers cpuA)
                in (cpuA { registers = setReg x (m + n) (registers cpuA), ip = ip cpuA + 1}, cpuB)
            Multiply (Register x) op ->
                let m = getReg x (registers cpuA)
                    n = resolve op (registers cpuA)
                in (cpuA { registers = setReg x (m * n) (registers cpuA), ip = ip cpuA + 1}, cpuB)
            Modulo (Register x) op ->
                let m = getReg x (registers cpuA)
                    n = resolve op (registers cpuA)
                in (cpuA { registers = setReg x (m `mod` n) (registers cpuA), ip = ip cpuA + 1}, cpuB)
            Receive (Register x) ->
                case queue cpuA of
                    [] -> (cpuA { waiting = True}, cpuB)
                    (v:vs) ->
                        let regs' = setReg x v (registers cpuA)
                        in (cpuA { registers = regs', queue = vs, ip = ip cpuA + 1}, cpuB)
            JumpGZ op1 op2 ->
                let m = resolve op1 (registers cpuA)
                    offset = resolve op2 (registers cpuA)
                    ip' = if m > 0 then ip cpuA + offset else ip cpuA + 1
                in (cpuA { ip = ip' }, cpuB)
            _ -> error "Invalid instruction/register combination"

runDual :: Program -> (Cpu, Cpu)
runDual prog = loop (initCpu 0, initCpu 1)
  where
    loop (c0, c1)
        | waiting c0 && waiting c1 = (c0, c1)
        | otherwise =
            let (c0', c1') = step prog (c0, c1)
                (c1'', c0'') = step prog (c1', c0')
            in loop (c0'', c1'')
    initCpu pid = Cpu { registers = Map.singleton 'p' pid, ip = 0, queue = [], waiting = False, sends = 0 }

process :: String -> Int
process content =
    case parse file "" content of
        Left err  -> error (show err)
        Right prog ->
            let (_, cpu1) = runDual prog
            in sends cpu1

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
