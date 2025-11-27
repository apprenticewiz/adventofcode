module Main where

import Data.Bits
import qualified Data.Set as Set

fastStep :: Int -> Int
fastStep r2 = go (r2 .|. 65536) 6718165
  where
    go r4 r2' =
      let r2'' = (((r2' + (r4 .&. 255)) .&. 16777215) * 65899) .&. 16777215
      in if r4 < 256
           then r2''
           else go (r4 `quot` 256) r2''

runFastLoop :: Int -> [Int]
runFastLoop start = go Set.empty start []
  where
    go seen v acc =
      if Set.member v seen
        then reverse acc
        else let v' = fastStep v
             in go (Set.insert v seen) v' (v:acc)

main :: IO ()
main = do
    let vals = take 20 $ runFastLoop 0
    mapM_ print $ zip [0..] vals
    putStrLn $ "Total unique values: " ++ show (length $ runFastLoop 0)
    putStrLn $ "Last value: " ++ show (last $ runFastLoop 0)
