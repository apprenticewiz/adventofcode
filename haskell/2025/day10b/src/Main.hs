module Main ( main ) where

import Control.DeepSeq
import System.Clock
import System.Environment
import System.Exit
import System.IO
import System.Process
import Text.Parsec
import Text.Parsec.String

type Button = [Int]
type Joltage = Int
type Machine = ([Joltage], [Button])

fileParser :: Parser [Machine]
fileParser = lineParser `sepEndBy1` newline <* eof

lineParser :: Parser Machine
lineParser = do
    _ <- lightsParser
    _ <- spaces
    buttons <- buttonParser `sepEndBy` spaces
    joltages <- joltageParser
    return (joltages, buttons)

lightsParser :: Parser String
lightsParser = between (char '[') (char ']') (many (oneOf ".#"))

buttonParser :: Parser Button
buttonParser = between (char '(') (char ')') (integer `sepBy` char ',')

joltageParser :: Parser [Joltage]
joltageParser = between (char '{') (char '}') (integer `sepBy` char ',')

integer :: Parser Int
integer = read <$> many1 digit

minPresses :: Machine -> IO Int
minPresses (goals, buttons) = do
    let smtLib = generateSMT goals buttons
    output <- readCreateProcess (proc "z3" ["-in"]) { std_in = CreatePipe } smtLib
    return $ case lines output of
        [] -> error "Empty output from Z3"
        ls -> parseModel ls
  where
    parseModel ls =
        let allWords = concatMap words ls
            numbers = [read (filter (`elem` "0123456789") w) :: Int 
                      | w <- allWords
                      , any (`elem` "0123456789") w
                      , all (`elem` "0123456789()") w]
        in if null numbers
           then error $ "Could not parse objective from: " ++ unlines ls
           else last numbers

generateSMT :: [Int] -> [Button] -> String
generateSMT goals buttons =
    let numButtons = length buttons
        numCounters = length goals
        varDecls = unlines ["(declare-const x" ++ show i ++ " Int)" | i <- [0..numButtons-1]]
        nonNeg = unlines ["(assert (>= x" ++ show i ++ " 0))" | i <- [0..numButtons-1]]
        counterConstraints = unlines
            [ "(assert (= " ++ buildSum counterIdx ++ " " ++ show (goals !! counterIdx) ++ "))"
            | counterIdx <- [0..numCounters-1]]        
        buildSum counterIdx =
            let affectingButtons = [j | (j, button) <- zip [0..] buttons, counterIdx `elem` button]
            in if null affectingButtons
               then "0"
               else if length affectingButtons == 1
                    then "x" ++ show (head affectingButtons)
                    else "(+ " ++ unwords ["x" ++ show j | j <- affectingButtons] ++ ")"
        objective = "(minimize (+ " ++ unwords ["x" ++ show i | i <- [0..numButtons-1]] ++ "))"        
    in unlines
        [ "(set-logic LIA)"
        , "(set-option :produce-models true)"
        , varDecls
        , nonNeg
        , counterConstraints
        , objective
        , "(check-sat)"
        , "(get-objectives)"
        , "(exit)"
        ]

process :: String -> IO Int
process content =
    case parse fileParser "" content of
        Left err -> error (show err)
        Right machines -> do
            results <- mapM minPresses machines
            return $ sum results

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
            result <- process content
            result `deepseq` return ()
            end <- getTime Monotonic
            let elapsed = diffTimeSpec start end
            putStrLn $ "result = " ++ show result
            putStrLn $ "elapsed time: " ++ showTime elapsed
        _ -> usage progname
