module Main ( main ) where

import Control.Monad.State
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Parsec hiding (State)
import Text.Parsec.String (Parser)
import Control.DeepSeq
import System.Clock

data Cpu = Cpu
    { a  :: Int
    , b  :: Int
    , c  :: Int
    , d  :: Int
    , pc :: Int
    } deriving (Show, Eq)

data Instruction =
      CpyInt Int Char
    | CpyReg Char Char
    | Inc Char
    | Dec Char
    | JnzInt Int Int
    | JnzReg Char Int
    deriving (Show, Eq)

type Program = [Instruction]

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

file :: Parser Program
file = many (instruction <* optional endOfLine)

instruction :: Parser Instruction
instruction = try cpyInt
          <|> try cpyReg
          <|> try inc
          <|> try dec
          <|> try jnzInt
          <|> jnzReg

cpyInt :: Parser Instruction
cpyInt = do
    _ <- string "cpy "
    src <- many1 (oneOf "-0123456789")
    _ <- space
    CpyInt (read src) <$> letter

cpyReg :: Parser Instruction
cpyReg = do
    _ <- string "cpy "
    src <- letter
    _ <- space
    CpyReg src <$> letter

inc :: Parser Instruction
inc = do
    _ <- string "inc "
    Inc <$> letter

dec :: Parser Instruction
dec = do
    _ <- string "dec "
    Dec <$> letter

jnzInt :: Parser Instruction
jnzInt = do
    _ <- string "jnz "
    src <- many1 (oneOf "-0123456789")
    _ <- space
    offset <- many1 (oneOf "-0123456789")
    return $ JnzInt (read src) (read offset)

jnzReg :: Parser Instruction
jnzReg = do
    _ <- string "jnz "
    src <- letter
    _ <- space
    offset <- many1 (oneOf "-0123456789")
    return $ JnzReg src (read offset)

runProgram :: Program -> State Cpu Int
runProgram insns = do
    cpu <- get
    if pc cpu < 0 || pc cpu >= length insns
        then return (a cpu)
        else do
            let instr = insns !! pc cpu
            execute instr
            runProgram insns
    where
        execute :: Instruction -> State Cpu ()
        execute (CpyInt val reg) = do
            modify (\cpu' -> setReg cpu' reg val)
            modify (\cpu' -> cpu' { pc = pc cpu' + 1 })
        execute (CpyReg srcReg destReg) = do
            cpu <- get
            let val = getReg cpu srcReg
            modify (\cpu' -> setReg cpu' destReg val)
            modify (\cpu' -> cpu' { pc = pc cpu' + 1 })
        execute (Inc reg) = do
            modify (\cpu' -> setReg cpu' reg (getReg cpu' reg + 1))
            modify (\cpu' -> cpu' { pc = pc cpu' + 1 })
        execute (Dec reg) = do
            modify (\cpu' -> setReg cpu' reg (getReg cpu' reg - 1))
            modify (\cpu' -> cpu' { pc = pc cpu' + 1 })
        execute (JnzInt cond offset) = do
            let condVal = cond /= 0
            if condVal
                then modify (\cpu' -> cpu' { pc = pc cpu' + offset })
                else modify (\cpu' -> cpu' { pc = pc cpu' + 1 })
        execute (JnzReg condReg offset) = do
            cpu <- get
            let condVal = getReg cpu condReg /= 0
            if condVal
                then modify (\cpu' -> cpu' { pc = pc cpu' + offset })
                else modify (\cpu' -> cpu' { pc = pc cpu' + 1 })

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
        Right insns -> evalState (runProgram insns) initialCpu
    where
        initialCpu = Cpu { a = 0, b = 0, c = 0, d = 0, pc = 0 }


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
