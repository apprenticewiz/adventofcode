module IntCode ( Cpu(memory), initCpu, execute ) where

import Control.Monad.ST ( ST, runST )
import Data.IntMap.Strict ( IntMap )
import qualified Data.IntMap.Strict as IntMap
import Data.Vector.Unboxed ( Vector )
import qualified Data.Vector.Unboxed as Vector
import Data.Vector.Unboxed.Mutable ( MVector )
import qualified Data.Vector.Unboxed.Mutable as MVector

data Instruction =
    AddMem
  | MulMem
  | Halt
  deriving (Eq, Show)

type Memory = Vector Int

data Cpu = Cpu { memory :: Memory, ip :: Int }
  deriving (Eq, Show)

data CpuM s = CpuM { memoryM :: MVector s Int, ipM :: Int }

initCpu :: [Int] -> Cpu
initCpu mem = Cpu (Vector.fromList mem) 0

execute :: Cpu -> Cpu
execute (Cpu mem0 ip0) = runST $ do
    memM <- Vector.thaw mem0
    let cpuM = CpuM memM ip0
    cpuMFinal <- executeM cpuM
    memFinal <- Vector.freeze (memoryM cpuMFinal)
    pure Cpu { memory = memFinal, ip = ipM cpuMFinal }

executeM :: CpuM s -> ST s (CpuM s)
executeM (CpuM memM currentIp) = do
    let decodeTable = IntMap.fromList [
            (1, (AddMem, 3)),
            (2, (MulMem, 3)),
            (99, (Halt, 0))
          ] :: IntMap (Instruction, Int)
    opcode <- MVector.read memM currentIp
    let (instruction, numOperands) = decodeTable IntMap.! opcode
    case numOperands of
        0 -> zeroOpInst instruction
        3 -> threeOpInst instruction
        _ -> error "unreachable"
    where
        zeroOpInst instruction = do
            case instruction of
                Halt -> 
                    pure (CpuM memM currentIp)
                _ -> error "unreachable"

        threeOpInst instruction = do
            aAddr <- MVector.read memM (currentIp + 1)
            bAddr <- MVector.read memM (currentIp + 2)
            cAddr <- MVector.read memM (currentIp + 3)
            aVal <- MVector.read memM aAddr
            bVal <- MVector.read memM bAddr
            case instruction of
                AddMem -> do
                    MVector.write memM cAddr (aVal + bVal)
                MulMem -> do
                    MVector.write memM cAddr (aVal * bVal)
                _ -> error "unreachable"
            executeM (CpuM memM (currentIp + 4))
