module Main ( main ) where

import Data.Ratio ( (%), denominator, numerator )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )
import Text.Regex.TDFA ( (=~), AllTextMatches(getAllTextMatches) )

type Equations = ((Integer, Integer, Integer), (Integer, Integer, Integer))

usage :: IO ()
usage = do
    progname <- getProgName
    putStrLn $ "usage: " ++ progname ++ " <file>"
    exitFailure

cramer :: Equations -> (Rational, Rational)
cramer ((a1, b1, c1), (a2, b2, c2)) = (x, y)
    where x = (b2 * c1 - b1 * c2) % (a1 * b2 - a2 * b1)
          y = (a1 * c2 - a2 * c1) % (a1 * b2 - a2 * b1)

parseInput :: [String] -> [Equations]
parseInput contentLines = parseInput' contentLines []
    where parseInput' [buttonA, buttonB, prize] systems = parseEquations (buttonA, buttonB, prize):systems
          parseInput' (buttonA:buttonB:prize:blank:rest) systems =
                (parseEquations (buttonA, buttonB, prize):systems) ++ parseInput' rest systems
          parseEquations (buttonA, buttonB, prize) =
                let buttonAVals = getAllTextMatches (buttonA =~ "[0-9]+") :: [String]
                    buttonBVals = getAllTextMatches (buttonB =~ "[0-9]+") :: [String]
                    prizeVals = getAllTextMatches (prize =~ "[0-9]+") :: [String]
                    a1 = read (head buttonAVals)
                    a2 = read (buttonAVals !! 1)
                    b1 = read (head buttonBVals)
                    b2 = read (buttonBVals !! 1)
                    c1 = read (head prizeVals) + 10000000000000
                    c2 = read (prizeVals !! 1) + 10000000000000
                in ((a1, b1, c1), (a2, b2, c2))

canSolve :: Equations -> Bool
canSolve equations =
    let (a, b) = cramer equations
        isWhole x = denominator x == 1
    in isWhole a && isWhole b

process :: String -> Integer
process contents =
    let equations = parseInput (lines contents)
    in foldl
        (\acc eqns ->
            let (ar, br) = cramer eqns
                (a, b) = (numerator ar, numerator br)
            in if canSolve eqns
                then acc + (a * 3 + b)
                else acc
        )
        0
        equations

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            contents <- readFile filename
            let result = process contents
            putStrLn $ "result = " ++ show result
        _ -> usage
