module IntCode ( Cpu(memory), initCpu, execute ) where

import Control.Monad.ST ( ST, runST )
import Data.IntMap.Strict ( IntMap )
import qualified Data.IntMap.Strict as IntMap
import Data.Vector.Unboxed ( Vector )
import qualified Data.Vector.Unboxed as Vector
import Data.Vector.Unboxed.Mutable ( MVector )
import qualified Data.Vector.Unboxed.Mutable as MVector

data Operation =
    Add
  | Multiply
  | Input
  | Output
  | Halt
  deriving (Eq, Show)

opDecodeTable :: IntMap (Operation, Int)
opDecodeTable = IntMap.fromList
    [ (1, (Add, 3))
    , (2, (Multiply, 3))
    , (3, (Input, 1))
    , (4, (Output, 1))
    , (99, (Halt, 0))
    ]

data Parameter =
    Position Int
  | Immediate Int
  deriving (Eq, Show)

paramDecodeTable :: IntMap Parameter
paramDecodeTable = IntMap.fromList
    [ (0, Position 0)
    , (1, Immediate 0)
    ]

type Instruction = (Operation, [Parameter])

type Memory = Vector Int

data Cpu = Cpu { memory :: Memory, ip :: Int, inputValue :: Int }
  deriving (Eq, Show)

data CpuM s = CpuM (MVector s Int) Int Int

initCpu :: [Int] -> Int -> Cpu
initCpu mem = Cpu (Vector.fromList mem) 0

execute :: Cpu -> [Int]
execute (Cpu mem0 ip0 inputVal) = runST $ do
    memM <- Vector.thaw mem0
    let cpuM = CpuM memM ip0 inputVal
    executeM cpuM []

executeM :: CpuM s -> [Int] -> ST s [Int]
executeM cpu currentOutputs = do
    instruction <- fetchAndDecode cpu
    executeInstruction cpu instruction currentOutputs
  where
        fetchAndDecode :: CpuM s -> ST s Instruction
        fetchAndDecode (CpuM mem pc inputVal) = do
            opcode <- MVector.read mem pc
            let opnum = opcode `mod` 100
            case IntMap.lookup opnum opDecodeTable of
                Nothing -> error $ "Invalid opcode " ++ show opcode ++ " (opnum=" ++ show opnum ++ ") at position " ++ show pc
                Just (operation, numOperands) -> do
                    parameters <- decodeParameters (CpuM mem pc inputVal) opcode numOperands
                    pure (operation, parameters)

        decodeParameters :: CpuM s -> Int -> Int -> ST s [Parameter]
        decodeParameters (CpuM mem pc _) opcode numOperands = 
            mapM readParam [1..numOperands]
          where
            readParam offset = do
                let paramCode = opcode `div` (10 ^ (offset + 1)) `mod` 10
                    paramType = paramDecodeTable IntMap.! paramCode
                case paramType of
                    Immediate _ -> Immediate <$> MVector.read mem (pc + offset)
                    Position _ -> Position <$> MVector.read mem (pc + offset)

        executeInstruction :: CpuM s -> Instruction -> [Int] -> ST s [Int]
        executeInstruction (CpuM mem pc inputVal) (operation, parameters) outputs = do
            case operation of
                Add -> do
                    case parameters of
                        [a, b, Position cVal] -> do
                            aVal <- case a of
                                    Immediate val -> pure val
                                    Position val -> MVector.read mem val
                            bVal <- case b of
                                    Immediate val -> pure val
                                    Position val -> MVector.read mem val
                            MVector.write mem cVal (aVal + bVal)
                            executeM (CpuM mem (pc + 4) inputVal) outputs
                        _ -> error $ "executionInstruction: invalid parameters for Add: " ++ show parameters
                Multiply -> do
                    case parameters of
                        [a, b, Position cVal] -> do
                            aVal <- case a of
                                    Immediate val -> pure val
                                    Position val -> MVector.read mem val
                            bVal <- case b of
                                    Immediate val -> pure val
                                    Position val -> MVector.read mem val
                            MVector.write mem cVal (aVal * bVal)
                            executeM (CpuM mem (pc + 4) inputVal) outputs
                        _ -> error $ "executionInstruction: invalid parameters for Multiply: " ++ show parameters
                Input -> do
                    case parameters of
                        [Position inputAddr] -> do
                            MVector.write mem inputAddr inputVal
                            executeM (CpuM mem (pc + 2) inputVal) outputs
                        _ -> error $ "executionInstruction: invalid parameters for Input: " ++ show parameters
                Output -> do
                    case parameters of
                        [param] -> do
                            output <- case param of
                                Immediate val -> pure val
                                Position addr -> MVector.read mem addr
                            executeM (CpuM mem (pc + 2) inputVal) (output : outputs)
                        _ -> error $ "executionInstruction: invalid parameters for Output " ++ show parameters
                Halt -> pure outputs
