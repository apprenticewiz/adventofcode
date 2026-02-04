module Main ( main ) where

import Data.Bits
import System.Environment
import System.Exit
import System.IO
import Text.Parsec
import Text.Parsec.String
import Control.DeepSeq
import System.Clock

type Registers = [Int]

data Instruction = Instruction
  { opcode  :: Int
  , source1 :: Int
  , source2 :: Int
  , dest    :: Int
  }
  deriving (Show)

type Sample = (Registers, Instruction, Registers)

file :: Parser [Sample]
file = many sample <* newline

sample :: Parser Sample
sample = do
    _ <- string "Before: "
    before <- registers
    _ <- newline
    intVals <- ints
    _ <- newline
    _ <- string "After: "
    optional (char ' ')
    after <- registers
    _ <- newline >> newline
    let [o, a, b, c] = intVals
    return (before, Instruction o a b c, after)

registers :: Parser [Int]
registers =
    between (char '[') (char ']') (int `sepBy` string ", ")

int :: Parser Int
int = read <$> many1 digit

ints :: Parser [Int]
ints = int `sepEndBy1` char ' '

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

applyArithOpRR :: (Int -> Int -> Int) -> Registers -> Instruction -> Registers
applyArithOpRR op regs insn =
    let src1 = source1 insn
        src2 = source2 insn
        dst = dest insn
        a = regs !! src1
        b = regs !! src2
        n = a `op` b
    in take dst regs ++ [n] ++ drop (dst + 1) regs

applyArithOpRI :: (Int -> Int -> Int) -> Registers -> Instruction -> Registers
applyArithOpRI op regs insn =
    let src1 = source1 insn
        src2 = source2 insn
        dst = dest insn
        a = regs !! src1
        b = src2
        n = a `op` b
    in take dst regs ++ [n] ++ drop (dst + 1) regs

addr :: Registers -> Instruction -> Registers
addr = applyArithOpRR (+)

addi :: Registers -> Instruction -> Registers
addi = applyArithOpRI (+)

mulr :: Registers -> Instruction -> Registers
mulr = applyArithOpRR (*)

muli :: Registers -> Instruction -> Registers
muli = applyArithOpRI (*)

banr :: Registers -> Instruction -> Registers
banr = applyArithOpRR (.&.)

bani :: Registers -> Instruction -> Registers
bani = applyArithOpRI (.&.)

borr :: Registers -> Instruction -> Registers
borr = applyArithOpRR (.|.)

bori :: Registers -> Instruction -> Registers
bori = applyArithOpRI (.|.)

setr :: Registers -> Instruction -> Registers
setr regs insn =
    let src1 = source1 insn
        dst = dest insn
        a = regs !! src1
    in take dst regs ++ [a] ++ drop (dst + 1) regs

seti :: Registers -> Instruction -> Registers
seti regs insn =
    let a = source1 insn
        dst = dest insn
    in take dst regs ++ [a] ++ drop (dst + 1) regs

applyCondOpIR :: (Int -> Int -> Bool) -> Registers -> Instruction -> Registers
applyCondOpIR op regs insn =
    let a = source1 insn
        src2 = source2 insn
        dst = dest insn
        b = regs !! src2
        n = if a `op` b then 1 else 0
    in take dst regs ++ [n] ++ drop (dst + 1) regs

applyCondOpRI :: (Int -> Int -> Bool) -> Registers -> Instruction -> Registers
applyCondOpRI op regs insn =
    let src1 = source1 insn
        a = regs !! src1
        b = source2 insn
        dst = dest insn
        n = if a `op` b then 1 else 0
    in take dst regs ++ [n] ++ drop (dst + 1) regs

applyCondOpRR :: (Int -> Int -> Bool) -> Registers -> Instruction -> Registers
applyCondOpRR op regs insn =
    let src1 = source1 insn
        src2 = source2 insn
        a = regs !! src1
        b = regs !! src2
        dst = dest insn
        n = if a `op` b then 1 else 0
    in take dst regs ++ [n] ++ drop (dst + 1) regs

gtir :: Registers -> Instruction -> Registers
gtir = applyCondOpIR (>)

gtri :: Registers -> Instruction -> Registers
gtri = applyCondOpRI (>)

gtrr :: Registers -> Instruction -> Registers
gtrr = applyCondOpRR (>)

eqir :: Registers -> Instruction -> Registers
eqir = applyCondOpIR (==)

eqri :: Registers -> Instruction -> Registers
eqri = applyCondOpRI (==)

eqrr :: Registers -> Instruction -> Registers
eqrr = applyCondOpRR (==)

ops :: [Registers -> Instruction -> Registers]
ops = [ addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr ]

checkSample :: Sample -> Bool
checkSample (before, insn, after) =
    let actual = map (\op -> op before insn) ops
        matches = filter (== after) actual
    in length matches >= 3

process :: String -> Int
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right samples -> length $ filter id $ map checkSample samples


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
