{-|
Module      : Ice40.Pll.Pad
Description : Ice40 PLL Pad hard IP primitive
Copyright   : (c) David Cox, 2021
License     : BSD 3-Clause
Maintainer  : standardsemiconductor@gmail.com

PLL Pad hard IP primitive from [Lattice Ice Technology Library](https://github.com/standardsemiconductor/VELDT-info/blob/master/SBTICETechnologyLibrary201708.pdf). The PLL pad primitive should be used when the source  clock of the PLL is driven by an input pad that is located in the bottom IO bank (IO Bank 2) or the top IO bank (IO Bank 0), and the source clock is not required inside the FPGA.
-}

module Ice40.Pll.Pad where

import Clash.Prelude
import Clash.Annotations.Primitive
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)

{-# ANN pllPadPrim (InlinePrimitive [Verilog] $ unindent [i|
  [  { "BlackBox" :
       { "name" : "Ice40.Pll.Pad.pllPadPrim"
       , "kind" : "Declaration"
       , "type" :
  "pllPadPrim
  :: KnownDomain dom'         -- ARG[0]
  -> BitVector 7              -- ARG[1]  divf
  -> BitVector 3              -- ARG[2]  divq
  -> BitVector 4              -- ARG[3]  divr
  -> String                   -- ARG[4]  feedbackPath
  -> BitVector 3              -- ARG[5]  filterRange
  -> String                   -- ARG[6]  pllOutSelect
  -> String                   -- ARG[7]  delayAdjustmentModeFeedback
  -> String                   -- ARG[8]  delayAdjustmentModeRelative
  -> BitVector 4              -- ARG[9]  fdaFeedback
  -> BitVector 4              -- ARG[10] fdaRelative
  -> Bit                      -- ARG[11] enableIceGate
  -> Clock dom                -- ARG[12] packagePin
  -> Signal dom (BitVector 8) -- ARG[13] dynamicDelay
  -> Signal dom Bit           -- ARG[14] resetb
  -> Signal dom Bit           -- ARG[15] bypass
  -> ( Clock dom'             -- pllOutCore
     , Clock dom'             -- globalOutCore
     , Signal dom' Bool       -- lock
     )"
       , "template" :
  "//SB_PLL40_PAD begin
  wire ~GENSYM[pllOutCore][0];
  wire ~GENSYM[globalOutCore][1];
  wire ~GENSYM[lock][2];

  SB_PLL40_PAD #(
    .DIVF                           (~ARG[1] ),
    .DIVQ                           (~ARG[2] ),
    .DIVR                           (~ARG[3] ),
    .FEEDBACK_PATH                  (~ARG[4] ),
    .FILTER_RANGE                   (~ARG[5] ),
    .PLLOUT_SELECT                  (~ARG[6] ),
    .DELAY_ADJUSTMENT_MODE_FEEDBACK (~ARG[7] ),
    .DELAY_ADJUSTMENT_MODE_RELATIVE (~ARG[8] ),
    .FDA_FEEDBACK                   (~ARG[9] ),
    .FDA_RELATIVE                   (~ARG[10]),
    .ENABLE_ICEGATE                 (~ARG[11])
  ) ~GENSYM[sb_pll40_core_inst][3] (
    .PACKAGEPIN                     (~ARG[12]),
    .DYNAMICDELAY                   (~ARG[13]),
    .RESETB                         (~ARG[14]),
    .BYPASS                         (~ARG[15]),
    .PLLOUTCORE                     (~SYM[0] ),
    .PLLOUTGLOBAL                   (~SYM[1] ),
    .LOCK                           (~SYM[2] )
  );

  assign ~RESULT = { ~SYM[0], ~SYM[1], ~SYM[2] };
  // SB_PLL40_PAD end"
      }
    }
  ]
  |]) #-}

-- | PLL Pad primitive
{-# NOINLINE pllPadPrim #-}
pllPadPrim 
  :: KnownDomain dom'         -- ARG[0]
  => BitVector 7              -- ^ divf
  -> BitVector 3              -- ^ divq
  -> BitVector 4              -- ^ divr
  -> String                   -- ^ feedbackPath
  -> BitVector 3              -- ^ filterRange
  -> String                   -- ^ pllOutSelect
  -> String                   -- ^ delayAdjustmentModeFeedback
  -> String                   -- ^ delayAdjustmentModeRelative
  -> BitVector 4              -- ^ fdaFeedback
  -> BitVector 4              -- ^ fdaRelative
  -> Bit                      -- ^ enableIceGate
  -> Clock dom                -- ^ packagePin
  -> Signal dom (BitVector 8) -- ^ dynamicDelay
  -> Signal dom Bit           -- ^ resetb
  -> Signal dom Bit           -- ^ bypass
  -> ( Clock dom'       -- pllOutCore
     , Clock dom'       -- globalOutCore
     , Signal dom' Bool -- lock
     )                  -- ^ (pllOutCore, globalOutCore, lock)
pllPadPrim !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ = (clockGen, clockGen, pure True)
