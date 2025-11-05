module Main (main) where

import Data.Array
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
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
    = Sound Operand
    | Set Operand Operand
    | Add Operand Operand
    | Multiply Operand Operand
    | Modulo Operand Operand
    | Recover Operand
    | JumpGZ Operand Operand
    deriving (Eq, Show)

type Program = Array Int Instruction

data Cpu = Cpu
    { registers :: Map Char Int
    , ip        :: Int
    , output    :: Maybe Int
    , recovered :: Bool
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
        "snd" -> Sound op
        "rcv" -> Recover op
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

step :: Program -> Cpu -> Cpu
step prog cpu
    | ip cpu < 0 || ip cpu > snd (bounds prog) = error "program counter out of bounds"
    | otherwise =
        case prog ! ip cpu of
            Sound (Register x) ->
                let val = getReg x (registers cpu)
                in cpu { output = Just val, ip = ip cpu + 1 }
            Set (Register x) op ->
                let val = resolve op (registers cpu)
                in cpu { registers = setReg x val (registers cpu), ip = ip cpu + 1 }
            Add (Register x) op ->
                let m = getReg x (registers cpu)
                    n = resolve op (registers cpu)
                in cpu { registers = setReg x (m + n) (registers cpu), ip = ip cpu + 1 }
            Multiply (Register x) op ->
                let m = getReg x (registers cpu)
                    n = resolve op (registers cpu)
                in cpu { registers = setReg x (m * n) (registers cpu), ip = ip cpu + 1 }
            Modulo (Register x) op ->
                let m = getReg x (registers cpu)
                    n = resolve op (registers cpu)
                in cpu { registers = setReg x (m `mod` n) (registers cpu), ip = ip cpu + 1 }
            Recover (Register x) ->
                let n = getReg x (registers cpu)
                in if n /= 0
                    then case output cpu of
                            Nothing -> error "recover: no sound played"
                            Just _ -> cpu { recovered = True, ip = ip cpu + 1 }
                    else cpu { ip = ip cpu + 1 }
            JumpGZ op1 op2 ->
                let m = resolve op1 (registers cpu)
                    offset = resolve op2 (registers cpu)
                in if m > 0
                    then cpu { ip = ip cpu + offset }
                    else cpu { ip = ip cpu + 1 }
            _ -> error "Invalid instruction/register combination"

runProgram :: Program -> Int
runProgram prog = loop initCpu
    where
        loop c
            | recovered c = fromJust (output c)
            | otherwise =
                let c' = step prog c
                in loop c'
        initCpu = Cpu { registers = Map.empty, ip = 0, output = Nothing, recovered = False }

process :: String -> Int
process content =
    case parse file "" content of
        Left err  -> error (show err)
        Right prog -> runProgram prog

usage :: String -> IO a
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

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
