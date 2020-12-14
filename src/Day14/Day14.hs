module Day14.Day14 where

import Control.Applicative (liftA2)
import Data.Bits (clearBit, setBit)
import qualified Data.Map as Map
import qualified Day14.Parser as Parser
import Day14.Types
import Lens.Simple (use, view, (%=), (.=))
import Types.Problem (Problem (..))

-- | The functions represented by the bitmask bit at the given index.
bitToFuns :: DecoderChip -> Int -> MaskBit -> [Int -> Int]
bitToFuns decChip bitIdx bit = case decChip of
  Ver1 -> case bit of
    X -> [id]
    Zero -> [(`clearBit` bitIdx)]
    One -> [(`setBit` bitIdx)]
  Ver2 -> case bit of
    X -> [(`clearBit` bitIdx), (`setBit` bitIdx)]
    Zero -> [id]
    One -> [(`setBit` bitIdx)]

-- | Turn a bitmask into a list of functions on Int.
maskToFuns :: DecoderChip -> Mask -> [Int -> Int]
maskToFuns decChip =
  foldr1 (liftA2 (.)) . zipWith (bitToFuns decChip) [0 ..] . reverse

-- | Program that runs a single bitmask instruction, when executed.
bitMaskInstrProg :: DecoderChip -> BitMaskInstr -> BitMaskProg
bitMaskInstrProg decChip instr = case instr of
  SetMask newMask -> mask .= newMask
  WriteToMem loc val -> do
    maskFuns <- maskToFuns decChip <$> use mask
    memory %= flip (foldr insert) maskFuns
    where
      insert maskFun = case decChip of
        Ver1 -> Map.insert loc (maskFun val)
        Ver2 -> Map.insert (maskFun loc) val

-- | Program that runs through a series of bitmask instructions, when executed.
prog :: DecoderChip -> [BitMaskInstr] -> BitMaskProg
prog decChip = mapM_ (bitMaskInstrProg decChip)

problem :: Problem
problem =
  Problem
    { parser = Parser.input,
      solvePartOne = sum . view memory . execBitMaskProg . prog Ver1,
      solvePartTwo = sum . view memory . execBitMaskProg . prog Ver2
    }
