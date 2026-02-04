module Main ( main ) where

import Control.Monad.State
import Data.Bits
import qualified Data.Set as Set
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
        in (Cpu (Vector.replicate 6 0) ipRegister 0, program)

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

fastStep :: Int -> Int
fastStep r2 = go (r2 .|. 65536) 6718165
  where
    go r4 !r2' =
      let r2'' = (((r2' + (r4 .&. 255)) .&. 16777215) * 65899) .&. 16777215
      in if r4 < 256
           then r2''
           else go (r4 `quot` 256) r2''

runFastLoop :: Int -> Int
runFastLoop start = go Set.empty start start
  where
    go seen lastV !v =
      if Set.member v seen
        then lastV
        else let v' = fastStep v
             in go (Set.insert v seen) v v'

runUntilSwitch :: Program -> State Cpu (Either Int Int)
runUntilSwitch program = loop
  where
    loop = do
      cpu <- get
      let pcVal = pc cpu
      if pcVal < 0 || pcVal >= Vector.length program
        then return (Right (-1))
        else do
          let ip = ipReg cpu
              regsWithPc = registers cpu // [(ip, pcVal)]
              inst = program ! pcVal
          if pcVal == 28
              then return (Left (regsWithPc ! 2))
              else do
                  cpu' <- execute inst regsWithPc cpu
                  put cpu'
                  loop

    execute inst regs cpu = do
      let cpu'  = cpu { registers = regs }
          cpu'' = executeInstruction inst cpu'
          newPc = (registers cpu'' ! ipReg cpu'') + 1
      return cpu'' { pc = newPc }

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

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

process :: String -> Int
process content =
    let (cpu, program) = parseInput content
    in case evalState (runUntilSwitch program) cpu of
        Right _ -> error "program terminated without hitting comparison instruction"
        Left firstR5 -> runFastLoop firstR5


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
