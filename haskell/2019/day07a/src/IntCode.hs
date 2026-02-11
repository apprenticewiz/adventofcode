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
  | JumpIfTrue
  | JumpIfFalse
  | LessThan
  | Equals
  | Halt
  deriving (Eq, Show)

opDecodeTable :: IntMap (Operation, Int)
opDecodeTable = IntMap.fromList
    [ (1, (Add, 3))
    , (2, (Multiply, 3))
    , (3, (Input, 1))
    , (4, (Output, 1))
    , (5, (JumpIfTrue, 2))
    , (6, (JumpIfFalse, 2))
    , (7, (LessThan, 3))
    , (8, (Equals, 3))
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

data Cpu = Cpu { memory :: Memory, ip :: Int, inputs :: [Int], inputIndex :: Int }
  deriving (Eq, Show)

data CpuM s = CpuM (MVector s Int) Int [Int] Int

initCpu :: [Int] -> [Int] -> Cpu
initCpu mem inputList = Cpu (Vector.fromList mem) 0 inputList 0

execute :: Cpu -> [Int]
execute (Cpu mem0 ip0 inputs0 inputIndex0) = runST $ do
    memM <- Vector.thaw mem0
    let cpuM = CpuM memM ip0 inputs0 inputIndex0
    executeM cpuM []

executeM :: CpuM s -> [Int] -> ST s [Int]
executeM cpu currentOutputs = do
    instruction <- fetchAndDecode cpu
    executeInstruction cpu instruction currentOutputs
  where
        fetchAndDecode :: CpuM s -> ST s Instruction
        fetchAndDecode (CpuM mem pc inps idx) = do
            opcode <- MVector.read mem pc
            let opnum = opcode `mod` 100
            case IntMap.lookup opnum opDecodeTable of
                Nothing -> error $ "Invalid opcode " ++ show opcode ++ " (opnum=" ++ show opnum ++ ") at position " ++ show pc
                Just (operation, numOperands) -> do
                    parameters <- decodeParameters (CpuM mem pc inps idx) opcode numOperands
                    pure (operation, parameters)

        decodeParameters :: CpuM s -> Int -> Int -> ST s [Parameter]
        decodeParameters (CpuM mem pc _ _) opcode numOperands =
            mapM readParam [1..numOperands]
          where
            readParam offset = do
                let paramCode = opcode `div` (10 ^ (offset + 1)) `mod` 10
                    paramType = paramDecodeTable IntMap.! paramCode
                case paramType of
                    Immediate _ -> Immediate <$> MVector.read mem (pc + offset)
                    Position _ -> Position <$> MVector.read mem (pc + offset)

        executeInstruction :: CpuM s -> Instruction -> [Int] -> ST s [Int]
        executeInstruction (CpuM mem pc inps idx) (operation, parameters) outputs = do
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
                            executeM (CpuM mem (pc + 4) inps idx) outputs
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
                            executeM (CpuM mem (pc + 4) inps idx) outputs
                        _ -> error $ "executionInstruction: invalid parameters for Multiply: " ++ show parameters
                Input -> do
                    case parameters of
                        [Position inputAddr] -> do
                            let inputVal = inps !! idx
                            MVector.write mem inputAddr inputVal
                            executeM (CpuM mem (pc + 2) inps (idx + 1)) outputs
                        _ -> error $ "executionInstruction: invalid parameters for Input: " ++ show parameters
                Output -> do
                    case parameters of
                        [param] -> do
                            output <- case param of
                                Immediate val -> pure val
                                Position addr -> MVector.read mem addr
                            executeM (CpuM mem (pc + 2) inps idx) (output : outputs)
                        _ -> error $ "executionInstruction: invalid parameters for Output: " ++ show parameters
                JumpIfTrue -> do
                    case parameters of
                        [a, b] -> do
                            aVal <- case a of
                                    Immediate val -> pure val
                                    Position val -> MVector.read mem val
                            bVal <- case b of
                                    Immediate val -> pure val
                                    Position val -> MVector.read mem val
                            if aVal /= 0 then
                                executeM (CpuM mem bVal inps idx) outputs
                            else
                                executeM (CpuM mem (pc + 3) inps idx) outputs
                        _ -> error $ "executionInstruction: invalid parameters for JumpIfTrue: " ++ show parameters
                JumpIfFalse -> do
                    case parameters of
                        [a, b] -> do
                            aVal <- case a of
                                    Immediate val -> pure val
                                    Position val -> MVector.read mem val
                            bVal <- case b of
                                    Immediate val -> pure val
                                    Position val -> MVector.read mem val
                            if aVal == 0 then
                                executeM (CpuM mem bVal inps idx) outputs
                            else
                                executeM (CpuM mem (pc + 3) inps idx) outputs
                        _ -> error $ "executionInstruction: invalid parameters for JumpIfFalse: " ++ show parameters
                LessThan -> do
                    case parameters of
                        [a, b, Position cVal] -> do
                            aVal <- case a of
                                    Immediate val -> pure val
                                    Position val -> MVector.read mem val
                            bVal <- case b of
                                    Immediate val -> pure val
                                    Position val -> MVector.read mem val
                            MVector.write mem cVal (if aVal < bVal then 1 else 0)
                            executeM (CpuM mem (pc + 4) inps idx) outputs
                        _ -> error $ "executionInstruction: invalid parameters for LessThan: " ++ show parameters
                Equals -> do
                    case parameters of
                        [a, b, Position cVal] -> do
                            aVal <- case a of
                                    Immediate val -> pure val
                                    Position val -> MVector.read mem val
                            bVal <- case b of
                                    Immediate val -> pure val
                                    Position val -> MVector.read mem val
                            MVector.write mem cVal (if aVal == bVal then 1 else 0)
                            executeM (CpuM mem (pc + 4) inps idx) outputs
                        _ -> error $ "executionInstruction: invalid parameters for Equals: " ++ show parameters
                Halt -> pure outputs
