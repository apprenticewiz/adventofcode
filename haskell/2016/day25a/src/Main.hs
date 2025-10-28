module Main ( main ) where

import Control.Monad.State
import Data.Array
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Parsec hiding (State)
import Text.Parsec.String (Parser)

data Cpu = Cpu
    { a  :: Int
    , b  :: Int
    , c  :: Int
    , d  :: Int
    , pc :: Int
    } deriving (Show, Eq)

data Operand = Immediate Int
             | Register Char
             deriving (Show, Eq)

data Instruction = Cpy Operand Operand
                 | Inc Operand
                 | Dec Operand
                 | Jnz Operand Operand
                 | Out Operand
                 deriving (Show, Eq)

type Program = Array Int Instruction

type Signals = [Int]

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

file :: Parser [Instruction]
file = instruction `sepEndBy` newline <* eof

instruction :: Parser Instruction
instruction = try oneOpInst <|> twoOpInst

oneOpInst :: Parser Instruction
oneOpInst = do
            mnemonic <- choice [ string "inc", string "dec", string "out" ]
            op <- spaces *> operand
            case mnemonic of
                "inc" -> pure (Inc op)
                "dec" -> pure (Dec op)
                "out" -> pure (Out op)
                _     -> error "unreachable"

twoOpInst :: Parser Instruction
twoOpInst = do
            mnemonic <- choice [ string "cpy", string "jnz" ]
            op1 <- spaces *> operand
            op2 <- spaces *> operand
            case mnemonic of
                "cpy" -> pure (Cpy op1 op2)
                "jnz" -> pure (Jnz op1 op2)
                _     -> error "unreachable"

operand :: Parser Operand
operand = immOp <|> regOp

immOp :: Parser Operand
immOp = do
        sgn <- option ' ' (char '-')
        nat <- many1 digit
        let val = read (sgn : nat)
        pure (Immediate val)

regOp :: Parser Operand
regOp = do
        reg <- oneOf "abcd"
        pure (Register reg)

runProgram :: State (Cpu, Program, Signals) Bool
runProgram = do
    (cpu, program, signals) <- get
    let (start, end) = bounds program
        maxSignals = 20  -- how many alternating outputs we check
    if pc cpu < start || pc cpu > end
        then return False
        else if length signals >= maxSignals
            then return (isAlternating (take maxSignals signals))
            else do
                let instr = program ! pc cpu
                execute instr
                runProgram
  where
    -- Check for alternating 0/1 pattern
    isAlternating :: [Int] -> Bool
    isAlternating [] = True
    isAlternating [_] = True
    isAlternating (x:y:rest) = x /= y && isAlternating (y:rest)

    -- Instruction execution
    execute :: Instruction -> State (Cpu, Program, Signals) ()
    execute (Cpy op (Register dst)) = do
        case op of
            Immediate val -> modify (\(cpu', prog, s) -> (setReg cpu' dst val, prog, s))
            Register src  -> modify (\(cpu', prog, s) -> (setReg cpu' dst (getReg cpu' src), prog, s))
        modify (\(cpu', prog, s) -> (cpu' { pc = pc cpu' + 1 }, prog, s))

    -- Optimized increment with multiply detection
    execute (Inc (Register r)) = do
        (cpu, prog, s) <- get
        let currPC = pc cpu
            (start, end) = bounds prog
        if currPC + 4 <= end
          then do
              let instWindow = [prog ! n | n <- [currPC .. currPC + 4]]
              case instWindow of
                -- detect pattern: inc X; dec Y; jnz Y -2; dec Z; jnz Z -5
                (Inc (Register x)
                 : Dec (Register y1)
                 : Jnz (Register y2) (Immediate (-2))
                 : Dec (Register z1)
                 : Jnz (Register z2) (Immediate (-5))
                 : _)
                  | y1 == y2 && z1 == z2 -> do
                      let n1 = getReg cpu y1
                          n2 = getReg cpu z1
                          updated = setReg cpu x (getReg cpu x + (n1 * n2))
                          cleared = setReg (setReg updated y1 0) z1 0
                      modify (\(_, p, s') -> (cleared { pc = currPC + 5 }, p, s'))
                _ -> do
                      modify (\(cpu', p, s') -> (setReg cpu' r (getReg cpu' r + 1), p, s'))
                      modify (\(cpu', p, s') -> (cpu' { pc = pc cpu' + 1 }, p, s'))
          else do
              modify (\(cpu', p, s') -> (setReg cpu' r (getReg cpu' r + 1), p, s'))
              modify (\(cpu', p, s') -> (cpu' { pc = pc cpu' + 1 }, p, s'))

    execute (Dec (Register r)) = do
        modify (\(cpu', prog, s) -> (setReg cpu' r (getReg cpu' r - 1), prog, s))
        modify (\(cpu', prog, s) -> (cpu' { pc = pc cpu' + 1 }, prog, s))

    execute (Jnz op1 op2) = do
        (cpu, prog, s) <- get
        let cond = case op1 of
                        Immediate n -> n
                        Register r  -> getReg cpu r
            offset = case op2 of
                        Immediate n -> n
                        Register r  -> getReg cpu r
        if cond /= 0
            then modify (\(cpu', prog', s') -> (cpu' { pc = pc cpu' + offset }, prog', s'))
            else modify (\(cpu', prog', s') -> (cpu' { pc = pc cpu' + 1 }, prog', s'))

    execute (Out op) = do
        (cpu, prog, s) <- get
        let val = case op of
                        Immediate n -> n
                        Register r  -> getReg cpu r
        modify (\(cpu', prog', s') -> (cpu' { pc = pc cpu' + 1 }, prog', s' ++ [val]))

    execute _ = modify (\(cpu', prog', s) -> (cpu' { pc = pc cpu' + 1 }, prog', s))

    -- Register utilities
    getReg cpu 'a' = a cpu
    getReg cpu 'b' = b cpu
    getReg cpu 'c' = c cpu
    getReg cpu 'd' = d cpu
    getReg _ _     = error "Invalid register"

    setReg cpu 'a' val = cpu { a = val }
    setReg cpu 'b' val = cpu { b = val }
    setReg cpu 'c' val = cpu { c = val }
    setReg cpu 'd' val = cpu { d = val }
    setReg _ _ _       = error "Invalid register"

process :: String -> Int
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right insns ->
            let program = listArray (0, length insns - 1) insns
            in go program 1
    where
        go program n =
            let initialCpu = Cpu { a = n, b = 0, c = 0, d = 0, pc = 0 }
                result = evalState runProgram (initialCpu, program, [])
            in if result
               then n
               else go program (n + 1)

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
