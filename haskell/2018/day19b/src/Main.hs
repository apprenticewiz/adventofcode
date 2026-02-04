module Main ( main ) where

import Control.Monad.State
import Data.Bits
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as Vector
import System.Environment
import System.Exit
import System.IO
import Control.DeepSeq
import System.Clock

data Cpu = Cpu
    { registers :: Vector Int
    , ipReg :: Int
    , pc :: Int
    }
    deriving (Show)

data Instruction =
    AddR Int Int Int
  | AddI Int Int Int
  | MulR Int Int Int
  | MulI Int Int Int
  | BanR Int Int Int
  | BanI Int Int Int
  | BorR Int Int Int
  | BorI Int Int Int
  | SetR Int Int Int
  | SetI Int Int Int
  | GtIR Int Int Int
  | GtRI Int Int Int
  | GtRR Int Int Int
  | EqIR Int Int Int
  | EqRI Int Int Int
  | EqRR Int Int Int
  deriving (Show)

type Program = Vector Instruction

parseInput :: String -> (Cpu, Program)
parseInput content = case lines content of
    [] -> error "Empty input"
    (ipDirective:rest) ->
        let ipRegister = case words ipDirective of
                ["#ip", n] -> read n
                _ -> error "Invalid IP directive"
            program = Vector.fromList $ map parseInstruction rest
            initialRegs = Vector.replicate 6 0 Vector.// [(0, 1)]
        in (Cpu initialRegs ipRegister 0, program)

parseInstruction :: String -> Instruction
parseInstruction line = case words line of
    [op, a, b, c] -> case op of
        "addr" -> AddR (read a) (read b) (read c)
        "addi" -> AddI (read a) (read b) (read c)
        "mulr" -> MulR (read a) (read b) (read c)
        "muli" -> MulI (read a) (read b) (read c)
        "banr" -> BanR (read a) (read b) (read c)
        "bani" -> BanI (read a) (read b) (read c)
        "borr" -> BorR (read a) (read b) (read c)
        "bori" -> BorI (read a) (read b) (read c)
        "setr" -> SetR (read a) (read b) (read c)
        "seti" -> SetI (read a) (read b) (read c)
        "gtir" -> GtIR (read a) (read b) (read c)
        "gtri" -> GtRI (read a) (read b) (read c)
        "gtrr" -> GtRR (read a) (read b) (read c)
        "eqir" -> EqIR (read a) (read b) (read c)
        "eqri" -> EqRI (read a) (read b) (read c)
        "eqrr" -> EqRR (read a) (read b) (read c)
        _      -> error $ "Invalid instruction: " ++ op
    _ -> error $ "Invalid instruction format: " ++ line

runProgramLimited :: Int -> Program -> State Cpu (Maybe Int)
runProgramLimited maxSteps program = go 0
  where
    go :: Int -> State Cpu (Maybe Int)
    go steps
        | steps >= maxSteps = return Nothing
        | otherwise = do
            cpu <- get
            let currentPc = pc cpu
            if currentPc < 0 || currentPc >= Vector.length program
                then return (Just $ registers cpu ! 0)
                else do
                    let regsWithPc = registers cpu // [(ipReg cpu, currentPc)]
                        instruction = program ! currentPc
                        cpu' = cpu { registers = regsWithPc }
                        cpu'' = executeInstruction instruction cpu'
                        newPc = (registers cpu'' ! ipReg cpu'') + 1
                    put $ cpu'' { pc = newPc }
                    go (steps + 1)

runProgram :: Program -> State Cpu Int
runProgram program = go
  where
    go :: State Cpu Int
    go = do
        cpu <- get
        let currentPc = pc cpu
        if currentPc < 0 || currentPc >= Vector.length program
            then return (registers cpu ! 0)
            else do
                let regsWithPc = registers cpu // [(ipReg cpu, currentPc)]
                    instruction = program ! currentPc
                    cpu' = cpu { registers = regsWithPc }
                    cpu'' = executeInstruction instruction cpu'
                    newPc = (registers cpu'' ! ipReg cpu'') + 1
                put $ cpu'' { pc = newPc }                
                go

executeInstruction :: Instruction -> Cpu -> Cpu
executeInstruction instruction cpu =
    let regs = registers cpu
        getReg r = regs ! r
        setReg r val = cpu { registers = regs // [(r, val)] }
        boolToInt b = if b then 1 else 0
    in case instruction of
        AddR a b c -> setReg c (getReg a + getReg b)
        AddI a b c -> setReg c (getReg a + b)
        MulR a b c -> setReg c (getReg a * getReg b)
        MulI a b c -> setReg c (getReg a * b)
        BanR a b c -> setReg c (getReg a .&. getReg b)
        BanI a b c -> setReg c (getReg a .&. b)
        BorR a b c -> setReg c (getReg a .|. getReg b)
        BorI a b c -> setReg c (getReg a .|. b)
        SetR a _ c -> setReg c (getReg a)
        SetI a _ c -> setReg c a
        GtIR a b c -> setReg c (boolToInt $ a > getReg b)
        GtRI a b c -> setReg c (boolToInt $ getReg a > b)
        GtRR a b c -> setReg c (boolToInt $ getReg a > getReg b)
        EqIR a b c -> setReg c (boolToInt $ a == getReg b)
        EqRI a b c -> setReg c (boolToInt $ getReg a == b)
        EqRR a b c -> setReg c (boolToInt $ getReg a == getReg b)

sumOfDivisors :: Int -> Int
sumOfDivisors n = sum [i | i <- [1..n], n `mod` i == 0]

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

process :: String -> Int
process content =
    let (cpu, program) = parseInput content
        cpuAfterSetup = execState (runProgramLimited 100 program) cpu
        targetNum = Vector.maximum (registers cpuAfterSetup)
    in if targetNum > 10000
        then sumOfDivisors targetNum
        else evalState (runProgram program) cpu


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
