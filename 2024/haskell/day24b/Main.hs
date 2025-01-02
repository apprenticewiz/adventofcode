module Main ( main ) where

import Data.Bits ( (.&.), (.|.), xor )
import Data.Char ( isDigit )
import Data.List ( break, intercalate, isPrefixOf, sort )
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Set ( Set )
import qualified Data.Set as Set
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )

data GateOp = And | Or | Xor
    deriving (Eq, Ord, Show)

data Component = Gate GateOp (Set Component) | Wire String
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
            in (wire, Wire wire)
        parseGateLine line =
            let [in1, gate, in2, _, wire] = words line
            in case gate of
                "AND" -> (wire, Gate And (Set.fromList [Wire in1, Wire in2]))
                "OR" -> (wire, Gate Or (Set.fromList [Wire in1, Wire in2]))
                "XOR" -> (wire, Gate Xor (Set.fromList [Wire in1, Wire in2]))
                _ -> error "unexpected gate op"

resolveTree :: Map String Component -> String -> Component
resolveTree device wire =
    case Map.lookup wire device of
        Just (Gate op xs) ->
            let [a, b] = Set.toList xs
            in Gate op (Set.fromList [resolveTree device (extractWire a), resolveTree device (extractWire b)])
        Just a -> a
        Nothing -> error "unreachable"
    where extractWire (Wire a) = a
          extractWire _ = error "expected wire"

remainder :: Int -> Component
remainder bit
    | bit == 0 = Gate And (Set.fromList [Wire (x 0), Wire (y 0)])
    | otherwise = Gate Or (Set.fromList [Gate And (Set.fromList [Wire (x bit), Wire (y bit)]),
                                         Gate And (Set.fromList [Gate Xor (Set.fromList [Wire (x bit), Wire (y bit)]),
                                                   remainder (bit - 1)])])

adder :: Int -> Component
adder bit
    | bit == 0 = Gate Xor (Set.fromList [Wire (x 0), Wire (y 0)])
    | otherwise = Gate Xor (Set.fromList [Gate Xor (Set.fromList [Wire (x bit), Wire (y bit)]), remainder (bit - 1)])

x :: Int -> String
x bit = "x" ++ replicate (2 - length (show bit)) '0' ++ show bit

y :: Int -> String
y bit = "y" ++ replicate (2 - length (show bit)) '0' ++ show bit

findSwap :: Map Component String -> Component -> Component -> Maybe (String, String)
findSwap wires expected actual =
    case Map.lookup expected wires of
        Just a -> Just (a, wires Map.! actual)
        Nothing ->
            let (e1, e2) = case expected of
                                Gate _ xs ->
                                    let [p, q] = Set.toList xs
                                    in (p, q)
                                _ -> error "unreachable"
                (a1, a2) = case actual of
                                Gate _ xs ->
                                    let [p, q] = Set.toList xs
                                    in (p, q)
                                _ -> error "unreachable"
            in check e1 e2 a1 a2
    where
        check e1 e2 a1 a2
            | e1 == a1 = findSwap wires e2 a2
            | e1 == a2 = findSwap wires e2 a1
            | e2 == a1 = findSwap wires e1 a2
            | e2 == a2 = findSwap wires e1 a1
            | otherwise = Nothing

process :: String -> String
process contents =
    let device = parse (lines contents)
        zBits = sort $ filter (isPrefixOf "z") (Map.keys device)
        (swapped, _) =
            foldl
                (\(swapped, currentDevice) zBit ->
                    let bit = read $ filter isDigit zBit
                        expected = adder bit
                        actual = resolveTree currentDevice zBit
                    in if expected /= actual
                        then let wires = Map.fromList $ map (\wire -> (resolveTree currentDevice wire, wire)) (Map.keys currentDevice)
                            in case findSwap wires expected actual of
                                Just (a, b) ->
                                    let a' = currentDevice Map.! a
                                        b' = currentDevice Map.! b
                                    in (a:b:swapped, Map.insert b a' (Map.insert a b' currentDevice))
                                Nothing -> (swapped, currentDevice)
                        else (swapped, currentDevice)
                )
                ([], device)
                zBits
    in intercalate "," $ sort swapped

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            contents <- readFile filename
            let result = process contents
            putStrLn $ "result = " ++ result
        _ -> usage
