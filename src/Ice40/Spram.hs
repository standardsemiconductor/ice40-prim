{-|
Module      : Ice40.Spram
Description : Ice40 single-port RAM hard IP primitive
Copyright   : (c) David Cox, 2021
License     : BSD 3-Clause
Maintainer  : standardsemiconductor@gmail.com

SPRAM hard IP primitive from [Lattice Ice Technology Library](https://github.com/standardsemiconductor/VELDT-info/blob/master/SBTICETechnologyLibrary201708.pdf)
-}
module Ice40.Spram ( spramPrim ) where

import Clash.Prelude
import Clash.Annotations.Primitive
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)

{-# ANN spramPrim (InlinePrimitive [Verilog] $ unindent [i|
  [  { "BlackBox" :
       { "name" : "Ice40.Spram.spramPrim"
       , "kind" : "Declaration"
       , "type" :
  "spramPrim
  :: KnownDomain dom            -- ARG[0] 
  => Clock dom                  -- ARG[1]  clock
  -> Signal dom (BitVector 14)  -- ARG[2]  address
  -> Signal dom (BitVector 16)  -- ARG[3]  dataIn
  -> Signal dom (BitVector 4)   -- ARG[4]  maskWrEn
  -> Signal dom Bit             -- ARG[5]  wrEN
  -> Signal dom Bit             -- ARG[6]  chipSelect
  -> Signal dom Bit             -- ARG[7]  standBy
  -> Signal dom Bit             -- ARG[8]  sleep
  -> Signal dom Bit             -- ARG[9] powerOff
  -> Signal dom (BitVector 16)  -- dataOut"
       , "template" :
  "//SB_SPRAM begin
  SB_SPRAM256KA ~GENSYM[sb_spram_inst][0] (
    .ADDRESS   (~ARG[2]),
    .DATAIN    (~ARG[3]),
    .MASKWREN  (~ARG[4]),
    .WREN      (~ARG[5]),
    .CHIPSELECT(~ARG[6]),
    .CLOCK     (~ARG[1]),
    .STANDBY   (~ARG[7]),
    .SLEEP     (~ARG[8]),
    .POWEROFF  (~ARG[9]),
    .DATAOUT   (~RESULT)
  );
  //SB_SPRAM end"
       }
    }
  ]
  |]) #-}

data Nyb = Nyb3 | Nyb2 | Nyb1 | Nyb0

-- | Single port RAM primitive
{-# NOINLINE spramPrim #-}
spramPrim
  :: KnownDomain dom            -- ARG[0] 
  => Clock dom                  -- ^ clock
  -> Signal dom (BitVector 14)  -- ^ address
  -> Signal dom (BitVector 16)  -- ^ dataIn
  -> Signal dom (BitVector 4)   -- ^ maskWrEn
  -> Signal dom Bit             -- ^ wrEN
  -> Signal dom Bit             -- ^ chipSelect
  -> Signal dom Bit             -- ^ standBy
  -> Signal dom Bit             -- ^ sleep
  -> Signal dom Bit             -- ^ powerOff
  -> Signal dom (BitVector 16)  -- ^ dataOut
spramPrim clock address dataIn maskWrEn wrEn chipSelect !_ !_ !_
  = concat4 <$> nyb3 <*> nyb2 <*> nyb1 <*> nyb0
  where
    addressU = unpack <$> address
    concat4 a b c d = a ++# b ++# c ++# d
    ramEn = toEnable $ bitToBool <$> chipSelect
    nybRam = withClock clock $ withEnable ramEn (blockRamPow2 (repeat 0) addressU)
    nyb3 = nybRam wrM3
    nyb2 = nybRam wrM2
    nyb1 = nybRam wrM1
    nyb0 = nybRam wrM0
    wrM3 = writeGuard Nyb3 <$> dataIn <*> addressU <*> wrEn <*> maskWrEn
    wrM2 = writeGuard Nyb2 <$> dataIn <*> addressU <*> wrEn <*> maskWrEn
    wrM1 = writeGuard Nyb1 <$> dataIn <*> addressU <*> wrEn <*> maskWrEn
    wrM0 = writeGuard Nyb0 <$> dataIn <*> addressU <*> wrEn <*> maskWrEn
    nybSlice = \case
      Nyb3 -> slice d15 d12
      Nyb2 -> slice d11 d8
      Nyb1 -> slice d7  d4
      Nyb0 -> slice d3  d0
    nybMask = \case
      Nyb3 -> (!(3 :: Index 4))
      Nyb2 -> (!(2 :: Index 4))
      Nyb1 -> (!(1 :: Index 4))
      Nyb0 -> (!(0 :: Index 4))
    writeGuard n dIn addr en mask
      | bitToBool en && (not.bitToBool) (nybMask n mask) = Just (addr, nybSlice n dIn)
      | otherwise = Nothing
