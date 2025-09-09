module Main ( main ) where

import Data.Bits
import Data.Char (isDigit)
import Data.Int (Int32)
import qualified Data.Map as Map
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

data Operation = AssignOp String
               | NotOp String
               | AndOp String String
               | OrOp String String
               | LeftShiftOp String Int
               | RightShiftOp String Int

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

evaluate :: (Map.Map String Operation) -> (Map.Map String Word) -> String -> (Word, Map.Map String Word)
evaluate ops cache expr =
    if all (\c -> isDigit c) expr
        then (read expr, cache)
        else if Map.member expr cache
            then (cache Map.! expr, cache)
            else
                let op = ops Map.! expr
                    (r, cache''') = case op of
                                      AssignOp src -> evaluate ops cache src
                                      NotOp src -> 
                                          let (a, cache') = evaluate ops cache src
                                          in (complement a, cache')
                                      AndOp src1 src2 ->
                                          let (a, cache') = evaluate ops cache src1
                                              (b, cache'') = evaluate ops cache' src2
                                          in (a .&. b, cache'')
                                      OrOp src1 src2 ->
                                          let (a, cache') = evaluate ops cache src1
                                              (b, cache'') = evaluate ops cache' src2
                                          in (a .|. b, cache'')
                                      LeftShiftOp src amt ->
                                          let (a, cache') = evaluate ops cache src
                                          in (shift a amt, cache')
                                      RightShiftOp src amt ->
                                          let (a, cache') = evaluate ops cache src
                                          in (shift a (-amt), cache')
                    masked = r .&. 0xffff
                in (masked, Map.insert expr masked cache''')

process :: String -> Int32
process content =
    let operations =
            foldl
                (\acc line ->
                    let parts = words line
                    in case parts of
                        [src, "->", dest] -> (Map.insert dest (AssignOp src) acc)
                        ["NOT", src, "->", dest] -> (Map.insert dest (NotOp src) acc)
                        [src1, "AND", src2, "->", dest] -> (Map.insert dest (AndOp src1 src2) acc)
                        [src1, "OR", src2, "->", dest] -> (Map.insert dest (OrOp src1 src2) acc)
                        [src, "LSHIFT", amt, "->", dest] -> (Map.insert dest (LeftShiftOp src (read amt)) acc)
                        [src, "RSHIFT", amt, "->", dest] -> (Map.insert dest (RightShiftOp src (read amt)) acc)
                        _ -> error ("malformed input line: " ++ line)
                )
                Map.empty
                (lines content)
        (a, _) = evaluate operations (Map.empty) "a"
        newOps = Map.insert "b" (AssignOp (show a)) operations
    in fromIntegral $ toInteger $ fst $ evaluate newOps (Map.empty) "a"

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
