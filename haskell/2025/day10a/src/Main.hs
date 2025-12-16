module Main ( main ) where

import Control.DeepSeq
import Data.Bits
import System.Clock
import System.Environment
import System.Exit
import System.IO
import Text.Parsec
import Text.Parsec.String

type Lights = Int

type Button = Int

type Machine = (Lights, [Button])

fileParser :: Parser [Machine]
fileParser = lineParser `sepEndBy1` newline <* eof

lineParser :: Parser Machine
lineParser = do
    goal <- lightsParser
    _ <- spaces
    buttons <- buttonParser `sepEndBy` spaces
    _ <- joltageParser
    return (goal, buttons)

lightsParser :: Parser Lights
lightsParser = do
    lightChars <- between (char '[') (char ']') (many (oneOf ".#"))
    return $ foldl (\acc (i, ch) -> if ch == '#' then setBit acc i else acc) 0 (zip [0..] lightChars)

buttonParser :: Parser Button
buttonParser = do
    buttonInts <- between (char '(') (char ')') (integer `sepBy` (char ','))
    return $ foldl (\acc i -> setBit acc i) 0 buttonInts

joltageParser :: Parser [Int]
joltageParser = between (char '{') (char '}') (integer `sepBy` (char ','))

integer :: Parser Int
integer = read <$> many1 digit

bitIndices :: Int -> [Int]
bitIndices n = [ i | i <- [0..63], testBit n i ]

buttonsForLight :: Lights -> [Button] -> [(Int, Int)]
buttonsForLight goal buttons = [ (rowMask i, rhsBit i) | i <- [0..maxLight] ]
  where
    maxLight = maximum (0 : bitIndices goal ++ concatMap bitIndices buttons)
    rhsBit i = if testBit goal i then 1 else 0
    rowMask i = foldl (\acc (j, button) -> if testBit button i then setBit acc j else acc) 0 (zip [0..] buttons)

eliminate :: [(Int, Int)] -> Int -> ([(Int, Int)], [Int], [Int])
eliminate rows n = go 0 0 rows [] []
  where
    go :: Int -> Int -> [(Int, Int)] -> [Int] -> [Int] -> ([(Int, Int)], [Int], [Int])
    go col row rs pivots frees
      | col >= n = (rs, reverse pivots, reverse frees)
      | row >= length rs = go (col + 1) row rs pivots (col:frees)
      | otherwise =
          case findPivot col row rs of
              Nothing -> go (col + 1) row rs pivots (col:frees)
              Just pivotRow ->
                  let rs' = swapRows row pivotRow rs
                      rs'' = eliminateColumn row col rs'
                  in go (col + 1) (row + 1) rs'' (col:pivots) frees

    findPivot :: Int -> Int -> [(Int, Int)] -> Maybe Int
    findPivot col start rs =
        let candidates = [ r | r <- [start..length rs - 1]
                             , testBit (fst (rs !! r)) col ]
        in case candidates of
            []    -> Nothing
            (p:_) -> Just p

    swapRows i j rs
      | i == j    = rs
      | otherwise =
          let ri = rs !! i
              rj = rs !! j
          in take i rs ++ [rj] ++ take (j - i - 1) (drop (i + 1) rs) ++ [ri] ++ drop (j + 1) rs

    eliminateColumn :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
    eliminateColumn pivotRow pivotCol rs =
        let (pMask, pRhs) = rs !! pivotRow
        in [ if r == pivotRow
                 then rs !! r
                 else let (m, rhs) = rs !! r
                      in if testBit m pivotCol
                          then (m `xor` pMask, rhs `xor` pRhs)
                          else (m, rhs)
           | r <- [0..length rs - 1]
           ]

backSubstitute :: [(Int, Int)] -> [Int] -> [Int] -> Int
backSubstitute rows pivots frees =
    let xWithFrees = foldl setFreeVars 0 frees
    in foldl solvePivot xWithFrees pivots
  where
    setFreeVars acc _ = acc

    solvePivot acc pivotCol =
        let rhs = findRhs pivotCol
        in if rhs == 1 then setBit acc pivotCol else acc

    findRhs pivotCol =
        case [ rhs | (mask, rhs) <- rows, testBit mask pivotCol ] of
             []    -> 0
             (r:_) -> r

buildNullspaceBasis :: [(Int, Int)] -> [Int] -> [Int] -> [Int]
buildNullspaceBasis rows pivots frees = map buildVector frees
  where
    buildVector f =
        let xWithFree = foldl (setFree f) 0 frees
        in foldl (setPivot f) xWithFree pivots
      where
        setFree x acc freeCol = if freeCol == x then setBit acc freeCol else acc

        setPivot freeCol acc pivotCol =
            let coeff = findCoeff pivotCol freeCol
            in if coeff == 1 then setBit acc pivotCol else acc

        findCoeff pivotCol freeCol =
            case [ if testBit mask freeCol then 1 else 0 | (mask, _) <- rows, testBit mask pivotCol ] of
                []    -> 0
                (c:_) -> c

gaussianGF2 :: Lights -> [Button] -> (Int, [Int])
gaussianGF2 goal buttons =
    let rows               = buttonsForLight goal buttons
        n                  = length buttons
        (r, pivots, frees) = eliminate rows n
        x0                 = backSubstitute r pivots frees
        nullBasis          = buildNullspaceBasis r pivots frees
    in (x0, nullBasis)

minPresses :: Machine -> Int
minPresses (goal, buttons) =
    let (x0, basis) = gaussianGF2 goal buttons
    in minimum [ popCount (foldl xor x0 [ b | (i, b) <- zip [0..] basis, testBit s i])
               | s <- [0 .. (1 `shiftL` length basis) - 1]
               ]

process :: String -> Int
process content =
    case parse fileParser "" content of
        Left err -> error (show err)
        Right machines -> sum $ map minPresses machines

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

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
