{-|
Module      : Ice40.Led
Description : LED Ice40 hard IP primitive
Copyright   : (c) David Cox, 2021
License     : BSD 3-Clause
Maintainer  : standardsemiconductor@gmail.com

LED hard IP primitive from Lattice Ice Technology Library https://github.com/standardsemiconductor/VELDT-info/blob/master/SBTICETechnologyLibrary201708.pdf
-}
module Ice40.Led ( led ) where

import Clash.Prelude
import Clash.Annotations.Primitive
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)

{-# ANN ledPrim (InlinePrimitive [Verilog] $ unindent [i|
  [  { "BlackBox" :
       { "name" : "Ice40.Led.ledPrim"
       , "kind" : "Declaration"
       , "type" :
  "ledPrim
  :: Signal dom Bit       -- ARG[0]  leddcs
  -> Clock dom            -- ARG[1]  leddclk
  -> Signal dom Bit       -- ARG[2]  ledddat7
  -> Signal dom Bit       -- ARG[3]  ledddat6
  -> Signal dom Bit       -- ARG[4]  ledddat5
  -> Signal dom Bit       -- ARG[5]  ledddat4
  -> Signal dom Bit       -- ARG[6]  ledddat3
  -> Signal dom Bit       -- ARG[7]  ledddat2
  -> Signal dom Bit       -- ARG[8]  ledddat1
  -> Signal dom Bit       -- ARG[9]  ledddat0
  -> Signal dom Bit       -- ARG[10] leddaddr3
  -> Signal dom Bit       -- ARG[11] leddaddr2
  -> Signal dom Bit       -- ARG[12] leddaddr1
  -> Signal dom Bit       -- ARG[13] leddaddr0
  -> Signal dom Bool      -- ARG[14] leddden
  -> Signal dom Bool      -- ARG[15] leddexe
  -> Unbundled dom ( Bit  -- pwmOut0
                   , Bit  -- pwmOut1
                   , Bit  -- pwmOut2
                   , Bool -- leddon
                   )"
      , "template" :
  "//SB_LEDDA_IP begin
  wire ~GENSYM[pwmOut0][0];
  wire ~GENSYM[pwmOut1][1];
  wire ~GENSYM[pwmOut2][2];
  wire ~GENSYM[leddon][3];

  SB_LEDDA_IP SB_LEDDA_IP_INST (
    .LEDDCS    ( ~ARG[0]  ),
    .LEDDCLK   ( ~ARG[1]  ),
    .LEDDDAT7  ( ~ARG[2]  ),
    .LEDDDAT6  ( ~ARG[3]  ),
    .LEDDDAT5  ( ~ARG[4]  ),
    .LEDDDAT4  ( ~ARG[5]  ),
    .LEDDDAT3  ( ~ARG[6]  ),
    .LEDDDAT2  ( ~ARG[7]  ),
    .LEDDDAT1  ( ~ARG[8]  ),
    .LEDDDAT0  ( ~ARG[9]  ),
    .LEDDADDR3 ( ~ARG[10] ),
    .LEDDADDR2 ( ~ARG[11] ),
    .LEDDADDR1 ( ~ARG[12] ),
    .LEDDADDR0 ( ~ARG[13] ),
    .LEDDDEN   ( ~ARG[14] ),
    .LEDDEXE   ( ~ARG[15] ),

    .PWMOUT0   ( ~SYM[0]  ),
    .PWMOUT1   ( ~SYM[1]  ),
    .PWMOUT2   ( ~SYM[2]  ),
    .LEDDON    ( ~SYM[3]  )
  );

  assign ~RESULT = {~SYM[0], ~SYM[1], ~SYM[2], ~SYM[3]};
  //SB_LEDDA_IP end"
      }
    }
  ]
  |]) #-}

{-# NOINLINE ledPrim #-}
ledPrim
  :: Signal dom Bit       -- ARG[0]  leddcs - CS to write LEDD IP registers
  -> Clock dom            -- ARG[1]  leddclk - Clock to write LEDD IP registers
  -> Signal dom Bit       -- ARG[2]  ledddat7 - bit 7 data to write into the LEDD IP registers
  -> Signal dom Bit       -- ARG[3]  ledddat6 - bit 6 data to write into the LEDD IP registers
  -> Signal dom Bit       -- ARG[4]  ledddat5 - bit 5 data to write into the LEDD IP registers
  -> Signal dom Bit       -- ARG[5]  ledddat4 - bit 4 data to write into the LEDD IP registers
  -> Signal dom Bit       -- ARG[6]  ledddat3 - bit 3 data to write into the LEDD IP registers
  -> Signal dom Bit       -- ARG[7]  ledddat2
  -> Signal dom Bit       -- ARG[8]  ledddat1
  -> Signal dom Bit       -- ARG[9]  ledddat0
  -> Signal dom Bit       -- ARG[10] leddaddr3
  -> Signal dom Bit       -- ARG[11] leddaddr2
  -> Signal dom Bit       -- ARG[12] leddaddr1
  -> Signal dom Bit       -- ARG[13] leddaddr0
  -> Signal dom Bool      -- ARG[14] leddden
  -> Signal dom Bool      -- ARG[15] leddexe
  -> Unbundled dom ( Bit  -- pwmOut0
                   , Bit  -- pwmOut1
                   , Bit  -- pwmOut2
                   , Bool -- leddon
                   )
ledPrim !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ = (pure 0, pure 0, pure 0, pure False)

-- | SB_LEDDA_IP primitive generates the RGB PWM outputs for the RGB LED drivers. The IP contains registers that are programmed in byte the SCI bus interface signals.
led
  :: HiddenClock dom
  => Signal dom Bit           -- ^ cs - CS to write LEDD IP registers
  -> Signal dom (BitVector 8) -- ^ dat - data to write into the LEDD IP registers
  -> Signal dom (BitVector 4) -- ^ addr - LEDD IP register address
  -> Signal dom Bool          -- ^ en - data enable input to indicate data and address are stable
  -> Signal dom Bool          -- ^ exe - enable to IP to run the blinking sequence. When it is LOW, the sequence stops at the nearest OFF state
  -> Unbundled dom ( Bit 
                   , Bit 
                   , Bit 
                   , Bool
                   ) -- ^ (pwmOut0, pwmOut1, pwmOut2, LED on indicator)
led cs dat addr en exe = (pwmOut0, pwmOut1, pwmOut2, on)
  where
    (pwmOut0, pwmOut1, pwmOut2, on) = ledPrim cs
                                              hasClock
                                              (bitAt 7 dat)
                                              (bitAt 6 dat)
                                              (bitAt 5 dat)
                                              (bitAt 4 dat)
                                              (bitAt 3 dat)
                                              (bitAt 2 dat)
                                              (bitAt 1 dat)
                                              (bitAt 0 dat)
                                              (bitAt 3 addr)
                                              (bitAt 2 addr)
                                              (bitAt 1 addr)
                                              (bitAt 0 addr)
                                              en
                                              exe

bitAt :: KnownNat n => Index n -> Signal dom (BitVector n) -> Signal dom Bit
bitAt n = fmap (!n)
