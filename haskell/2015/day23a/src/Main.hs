module Main (main) where

import Control.DeepSeq
import Control.Monad.State
import qualified Data.Array as Array
import System.Clock
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Parsec hiding (State)
import Text.Parsec.String

data Register = A | B
              deriving (Eq, Show)

data Instruction = Half Register
                 | Triple Register
                 | Increment Register
                 | Jump Int
                 | JumpIfEven Register Int
                 | JumpIfOne Register Int
                 deriving (Eq, Show)

type Program = Array.Array Int Instruction

data CPU = CPU 
  { regA :: Int
  , regB :: Int
  , ip   :: Int
  }
  deriving (Eq, Show)

strToReg :: String -> Register
strToReg "a" = A
strToReg "b" = B
strToReg _   = error "invalid register"

file :: Parser [Instruction]
file = (instr `sepEndBy` newline) <* eof

instr :: Parser Instruction
instr = choice [ unaryReg, try uncJump, condJump ]

reg :: Parser Register
reg = do
      r <- string "a" <|> string "b"
      pure (strToReg r)

unaryReg :: Parser Instruction
unaryReg = do
           mne <- string "hlf" <|> string "tpl" <|> string "inc"
           spaces
           r <- reg
           let ins = case mne of
                         "hlf" -> Half r
                         "tpl" -> Triple r
                         "inc" -> Increment r
                         _     -> error "unreachable"
           pure ins

uncJump :: Parser Instruction
uncJump = do
          _ <- string "jmp" >> spaces
          sign <- oneOf "+-"
          offsetStr <- many1 digit
          let offset = case sign of
                           '+' -> read offsetStr
                           '-' -> -(read offsetStr)
                           _   -> error "unreachable"
          pure (Jump offset)

condJump :: Parser Instruction
condJump = do
           mne <- try (string "jie") <|> string "jio"
           spaces
           r <- reg
           _ <- string ","
           spaces
           sign <- oneOf "+-"
           offsetStr <- many1 digit
           let offset = case sign of
                            '+' -> read offsetStr
                            '-' -> -(read offsetStr)
                            _   -> error "unreachable"
           let ins = case mne of
                         "jie" -> JumpIfEven r offset
                         "jio" -> JumpIfOne r offset
                         _     -> error "unreachable"
           pure ins

usage :: String -> IO ()
usage progname = do
  hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
  exitFailure

initCPU :: CPU
initCPU = CPU { regA = 0, regB = 0, ip = 0 }

execute :: Program -> State CPU Int
execute prog = do
               cpu <- get
               let i = ip cpu
               let (f, l) = Array.bounds prog
               if i < f || i > l
                   then return (regB cpu)
                   else do
                        let newCPU = case prog Array.! i of
                                         Half r -> if r == A
                                                       then cpu { regA = (regA cpu) `div` 2, ip = i + 1 }
                                                       else cpu { regB = (regB cpu) `div` 2, ip = i + 1 }
                                         Triple r -> if r == A
                                                         then cpu { regA = (regA cpu) * 3, ip = i + 1 }
                                                         else cpu { regB = (regB cpu) * 3, ip = i + 1 }
                                         Increment r -> if r == A
                                                            then cpu { regA = (regA cpu) + 1, ip = i + 1 }
                                                            else cpu { regB = (regB cpu) + 1, ip = i + 1 }
                                         Jump n -> cpu { ip = i + n }
                                         JumpIfEven r n -> if r == A
                                                               then if even (regA cpu)
                                                                        then cpu { ip = i + n }
                                                                        else cpu { ip = i + 1 }
                                                               else if even (regB cpu)
                                                                        then cpu { ip = i + n }
                                                                        else cpu { ip = i + 1 }
                                         JumpIfOne r n -> if r == A
                                                              then if (regA cpu) == 1
                                                                       then cpu { ip = i + n }
                                                                       else cpu { ip = i + 1 }
                                                              else if (regB cpu) == 1
                                                                       then cpu { ip = i + n }
                                                                       else cpu { ip = i + 1 }
                        put newCPU
                        execute prog

process :: String -> Int
process content =
  case parse file "" content of
    Left err    -> error (show err)
    Right insns ->
        let prog = Array.array (0, length insns - 1) (zip [0..] insns)
        in fromIntegral $ evalState (execute prog) initCPU

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
      let result = (process content)
      result `deepseq` return ()
      end <- getTime Monotonic
      let elapsed = diffTimeSpec start end
      putStrLn $ "result = " ++ show result
      putStrLn $ "elapsed time: " ++ showTime elapsed
    _ -> usage progname

