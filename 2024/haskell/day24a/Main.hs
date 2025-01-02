module Main ( main ) where

import Data.Bits ( (.&.), (.|.), shiftL, xor )
import Data.Char ( isDigit )
import Data.List ( break, isPrefixOf )
import Data.Map ( Map )
import qualified Data.Map as Map
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )

data GateOp = And | Or | Xor
    deriving (Eq, Ord, Show)

data Component = Gate GateOp Component Component | Input Int | Wire String
    deriving (Eq, Ord, Show)

usage :: IO ()
usage = do
    progname <- getProgName
    putStrLn $ "usage: " ++ progname ++ " <file>"
    exitFailure

parse :: [String] -> Map String Component
parse contents =
    let (initialText, _:gatesText) = break (== "") contents
        initial = map parseInitialLine initialText
        gates = map parseGateLine gatesText
    in Map.fromList (initial ++ gates)
    where
        parseInitialLine line =
            let (wire, value) = (\xs -> (init $ head xs, xs !! 1)) (words line)
            in (wire, Input (read value :: Int))
        parseGateLine line =
            let [in1, gate, in2, _, wire] = words line
            in case gate of
                "AND" -> (wire, Gate And (Wire in1) (Wire in2))
                "OR" -> (wire, Gate Or (Wire in1) (Wire in2))
                "XOR" -> (wire, Gate Xor (Wire in1) (Wire in2))
                _ -> error "unexpected gate op"

resolve :: Map String Component -> String -> Int
resolve _device value
    | all isDigit value = read value
resolve device wire =
    case Map.lookup wire device of
        Just (Input value) -> value
        Just (Gate And in1 in2) -> resolve device (extractWire in1) .&. resolve device (extractWire in2)
        Just (Gate Or in1 in2) -> resolve device (extractWire in1) .|. resolve device (extractWire in2)
        Just (Gate Xor in1 in2) -> resolve device (extractWire in1) `xor` resolve device (extractWire in2)
        _ -> 0
    where
        extractWire (Wire s) = s
        extractWire _ = error "expected wire"

calculate :: Map String Component -> Int
calculate device =
    sum . map (\wire -> shiftL (resolve device wire) (read (tail wire)) :: Int)
    . filter (isPrefixOf "z") $ Map.keys device

process :: String -> Int
process contents =
    let device = parse (lines contents)
    in calculate device

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            contents <- readFile filename
            let result = process contents
            putStrLn $ "result = " ++ show result
        _ -> usage
