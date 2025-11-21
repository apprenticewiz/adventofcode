module Main ( main ) where

import Data.Bits
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Set ( Set )
import qualified Data.Set as Set
import System.Environment
import System.Exit
import System.IO
import Text.Parsec
import Text.Parsec.String

type Registers = [Int]

data Instruction = Instruction
  { opcode  :: Int
  , source1 :: Int
  , source2 :: Int
  , dest    :: Int
  }
  deriving (Show)

type Sample = (Registers, Instruction, Registers)

file :: Parser ([Sample], [Instruction])
file = do
    samples <- many sample
    _ <- count 2 newline
    program <- instruction `sepEndBy1` newline
    return (samples, program)

sample :: Parser Sample
sample = do
    _ <- string "Before: "
    before <- registers
    _ <- newline
    insn <- instruction
    _ <- newline
    _ <- string "After: "
    optional (char ' ')
    after <- registers
    _ <- newline >> newline
    return (before, insn, after)

instruction :: Parser Instruction
instruction = do
    intVals <- ints
    let [o, a, b, c] = intVals
    return (Instruction o a b c)

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

ops :: Map String (Registers -> Instruction -> Registers)
ops = Map.fromList
  [ ("addr", addr), ("addi", addi), ("mulr", mulr), ("muli", muli)
  , ("banr", banr), ("bani", bani), ("borr", borr), ("bori", bori)
  , ("setr", setr), ("seti", seti)
  , ("gtir", gtir), ("gtri", gtri), ("gtrr", gtrr)
  , ("eqir", eqir), ("eqri", eqri), ("eqrr", eqrr)
  ]

checkSample :: Sample -> Set String
checkSample (before, insn, after) =
    let actual = map (\opName -> let op = ops Map.! opName in (opName, op before insn)) (Map.keys ops)
        matches = map fst $ filter (\(_, result) -> result == after) actual
    in Set.fromList matches

buildPossiblesMap :: [Sample] -> Map Int (Set String)
buildPossiblesMap = foldr step Map.empty
  where
    step s@(_, insn, _) m =
        let oc = opcode insn
            possibles = checkSample s
        in Map.insertWith Set.intersection oc possibles m

canonicalize :: Map Int (Set String) -> Map Int String
canonicalize possibles = go possibles Map.empty
  where
    go poss solved
        | Map.null poss = solved
        | otherwise =
            let
                singles = Map.filter (\s -> Set.size s == 1) poss
            in if Map.null singles
                then error "Cannot resolve opcodes uniquely"
                else
                    let
                        (oc, singleSet) = head (Map.toList singles)
                        opName = head (Set.toList singleSet)
                        poss' = Map.delete oc poss
                        poss'' = Map.map (Set.delete opName) poss'
                        solved' = Map.insert oc opName solved
                    in go poss'' solved'

process :: String -> Int
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right samples ->
            let possibles = buildPossiblesMap (fst samples)
                opcodeTable = canonicalize possibles
                program = snd samples
                endRegisters = foldl' (applyInstruction opcodeTable) (replicate 4 0) program
            in endRegisters !! 0
  where
    applyInstruction :: Map Int String -> [Int] -> Instruction -> [Int]
    applyInstruction opcodeTable regs insn =
        let op = opcodeTable Map.! opcode insn
            opFn = ops Map.! op
        in opFn regs insn

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
