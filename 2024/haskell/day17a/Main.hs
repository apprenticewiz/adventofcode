module Main ( main ) where

import Data.Bits ( xor )
import Data.Text ( intercalate, pack, split, unpack )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )

data Computer = Computer { registerA :: Int
                         , registerB :: Int
                         , registerC :: Int
                         , instructionPointer :: Int
                         , outputList :: [Int]
                         }
                         deriving (Eq, Ord, Show)

type OpcodeFunction = Int -> Computer -> Computer
type Program = [Int]

usage :: IO ()
usage = do
    progname <- getProgName
    putStrLn $ "usage: " ++ progname ++ " <file>"
    exitFailure

initComputer :: Int -> Int -> Int -> Computer
initComputer a b c =
    Computer { registerA = a
             , registerB = b
             , registerC = c
             , instructionPointer = 0
             , outputList = []
             }

getComboValue :: Int -> Computer -> Int
getComboValue operand computer
    | operand `elem` [0..3] = operand
    | operand == 4 = registerA computer
    | operand == 5 = registerB computer
    | operand == 6 = registerC computer
    | otherwise = error ("invalid operand detected: " ++ show operand)

handleAdv :: OpcodeFunction
handleAdv operand computer =
    let numerator = registerA computer
        denominator = 2 ^ getComboValue operand computer
    in computer { registerA = numerator `div` denominator
                , instructionPointer = instructionPointer computer + 2
                }

handleBxl :: OpcodeFunction
handleBxl operand computer =
    computer { registerB = registerB computer `xor` operand
             , instructionPointer = instructionPointer computer + 2
             }

handleBst :: OpcodeFunction
handleBst operand computer =
    computer { registerB = getComboValue operand computer `mod` 8
             , instructionPointer = instructionPointer computer + 2
             }

handleJnz :: OpcodeFunction
handleJnz operand computer =
    if registerA computer == 0
        then computer { instructionPointer = instructionPointer computer + 2 }
        else computer { instructionPointer = operand }

handleBxc :: OpcodeFunction
handleBxc operand computer =
    computer { registerB = registerB computer `xor` registerC computer
             , instructionPointer = instructionPointer computer + 2
             }

handleOut :: OpcodeFunction
handleOut operand computer =
    computer { outputList = outputList computer ++ [getComboValue operand computer `mod` 8]
             , instructionPointer = instructionPointer computer + 2
             }

handleBdv :: OpcodeFunction
handleBdv operand computer =
    let numerator = registerA computer
        denominator = 2 ^ getComboValue operand computer
    in computer { registerB = numerator `div` denominator
                , instructionPointer = instructionPointer computer + 2
                }

handleCdv :: OpcodeFunction
handleCdv operand computer =
    let numerator = registerA computer
        denominator = 2 ^ getComboValue operand computer
    in computer { registerC = numerator `div` denominator
                , instructionPointer = instructionPointer computer + 2
                }

opcodeTable :: [OpcodeFunction]
opcodeTable = [ handleAdv
              , handleBxl
              , handleBst
              , handleJnz
              , handleBxc
              , handleOut
              , handleBdv
              , handleCdv
              ]

parseInput :: String -> (Computer, Program)
parseInput contents =
    let contentLines = lines contents
        line0 = head contentLines
        line1 = contentLines !! 1
        line2 = contentLines !! 2
        line4 = contentLines !! 4
        regA = read $ unpack $ (!! 1) $ split (== ':') $ pack line0 :: Int
        regB = read $ unpack $ (!! 1) $ split (== ':') $ pack line1 :: Int
        regC = read $ unpack $ (!! 1) $ split (== ':') $ pack line2 :: Int
        program = map (read . unpack) $ split (== ',') $ (!! 1) $ split (== ':') $ pack line4 :: [Int]
    in (initComputer regA regB regC, program)

execute :: Program -> Computer -> String
execute program computer =
    if instructionPointer computer >= length program
        then unpack $ (intercalate (pack ",") . map (pack .show)) (outputList computer)
        else
            let ip = instructionPointer computer
                opcode = program !! ip
                operand = program !! (ip + 1)
            in execute program ((opcodeTable !! opcode) operand computer)

process :: String -> String
process contents =
    let (computer, program) = parseInput contents
    in execute program computer

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            contents <- readFile filename
            let result = process contents
            putStrLn $ "result = " ++ result
        _ -> usage
