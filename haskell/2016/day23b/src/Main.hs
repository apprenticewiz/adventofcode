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
                 | Tgl Operand
                 deriving (Show, Eq)

type Program = Array Int Instruction

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
            mnemonic <- choice [ string "inc", string "dec", string "tgl" ]
            op <- spaces *> operand
            case mnemonic of
                "inc" -> pure (Inc op)
                "dec" -> pure (Dec op)
                "tgl" -> pure (Tgl op)
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
        let val = read ([sgn] ++ nat)
        pure (Immediate val)

regOp :: Parser Operand
regOp = do
        reg <- oneOf "abcd"
        pure (Register reg)

runProgram :: State (Cpu, Program) Int
runProgram = do
    (cpu, program) <- get
    let (start, end) = bounds program
    if pc cpu < start || pc cpu > end
        then return (a cpu)
        else do
            let instr = program ! pc cpu
            execute instr
            runProgram
    where
        execute :: Instruction -> State (Cpu, Program) ()
        execute (Cpy op (Register dst)) = do
            case op of
                Immediate val -> modify (\(cpu', prog) -> (setReg cpu' dst val, prog))
                Register src -> modify (\(cpu', prog) -> (setReg cpu' dst (getReg cpu' src), prog))
            modify (\(cpu', prog) -> (cpu' { pc = pc cpu' + 1 }, prog))
        execute (Inc (Register r)) = do
            (cpu, prog) <- get
            let currPC = pc cpu
                instWindow = [ prog ! n | n <- [currPC .. currPC + 4] ]
            case instWindow of
                (Inc (Register x):Dec (Register y1):Jnz (Register y2) (Immediate (-2)):
                 Dec (Register z1):Jnz (Register z2) (Immediate (-5)):_) | y1 == y2 && z1 == z2 -> do
                    let n1 = getReg cpu y1
                    let n2 = getReg cpu z1
                    modify (\(cpu', prog') -> (setReg cpu' x (getReg cpu' x + (n1 * n2)), prog'))
                    modify (\(cpu', prog') -> (cpu' { pc = currPC + 5 }, prog'))
                _ -> do
                    modify (\(cpu', prog') -> (setReg cpu' r (getReg cpu' r + 1), prog'))
                    modify (\(cpu', prog') -> (cpu' { pc = currPC + 1 }, prog'))
        execute (Dec (Register r)) = do
            modify (\(cpu', prog) -> (setReg cpu' r (getReg cpu' r - 1), prog))
            modify (\(cpu', prog) -> (cpu' { pc = pc cpu' + 1 }, prog))
        execute (Jnz op1 op2) = do
            (cpu, _) <- get
            let offset = case op2 of
                             Immediate n -> n
                             Register r2 -> getReg cpu r2
            case op1 of
                Immediate condVal -> do
                    if condVal /= 0
                        then modify (\(cpu', prog) -> (cpu' { pc = pc cpu' + offset }, prog))
                        else modify (\(cpu', prog) -> (cpu' { pc = pc cpu' + 1 }, prog))
                Register r1 -> do
                    let condVal = getReg cpu r1
                    if condVal /= 0
                        then modify (\(cpu', prog) -> (cpu' { pc = pc cpu' + offset }, prog))
                        else modify (\(cpu', prog) -> (cpu' { pc = pc cpu' + 1 }, prog))
        execute (Tgl op) = do
            (cpu, prog) <- get
            let offset = case op of
                             Immediate n -> n
                             Register r -> getReg cpu r
                (start, end) = bounds prog
                dest = pc cpu + offset
            if dest < start || dest > end
                then modify (\(cpu', prog') -> (cpu' { pc = pc cpu' + 1}, prog'))
                else do
                     let newInst = (case prog ! dest of
                                        Cpy op1 op2 -> Jnz op1 op2
                                        Inc op1 -> Dec op1
                                        Dec op1 -> Inc op1
                                        Jnz op1 op2 -> Cpy op1 op2
                                        Tgl op1 -> Inc op1)
                     modify (\(cpu', prog') -> (cpu' { pc = pc cpu' + 1 }, prog' // [(dest, newInst)]))
        execute _ = do
                    modify (\(cpu', prog') -> (cpu' { pc = pc cpu' + 1 }, prog'))

        getReg :: Cpu -> Char -> Int
        getReg cpu 'a' = a cpu
        getReg cpu 'b' = b cpu
        getReg cpu 'c' = c cpu
        getReg cpu 'd' = d cpu
        getReg _ _     = error "Invalid register"

        setReg :: Cpu -> Char -> Int -> Cpu
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
            in evalState runProgram (initialCpu, program)
    where
        initialCpu = Cpu { a = 12, b = 0, c = 0, d = 0, pc = 0 }

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
