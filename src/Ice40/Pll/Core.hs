{-|
Module      : Ice40.Pll.Core
Description : Ice40 PLL hard IP primitive
Copyright   : (c) David Cox, 2021-2024
License     : BSD 3-Clause
Maintainer  : standardsemiconductor@gmail.com
PLL Core hard IP primitive from [Lattice Ice Technology Library](https://github.com/standardsemiconductor/VELDT-info/blob/master/SBTICETechnologyLibrary201708.pdf). The PLL core primitive should be used when the source clock of the PLL is driven by FPGA routing i.e. when the PLL source clock originates on the FPGA or is driven by an input pad the is not in the bottom IO bank (IO Bank 2).
-}
module Ice40.Pll.Core where

import Clash.Prelude
import Clash.Annotations.Primitive
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)

{-# ANN pllCorePrim (InlinePrimitive [Verilog] $ unindent [i|
  [  { "BlackBox" :
       { "name" : "Ice40.Pll.Core.pllCorePrim"
       , "kind" : "Declaration"
       , "type" :
  "pllCorePrim
  :: KnownDomain dom'         -- ARG[0]
  => BitVector 7              -- ARG[1]  divf
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
  -> Clock dom                -- ARG[12] referenceClk
  -> Signal dom (BitVector 8) -- ARG[13] dynamicDelay
  -> Signal dom Bit           -- ARG[14] resetb
  -> Signal dom Bit           -- ARG[15] bypass
  -> ( Clock dom'             -- pllOutCore
     , Clock dom'             -- globalOutCore
     , Signal dom' Bool       -- lock
     )"
      , "template" :
  "//SB_PLL40_CORE begin
  wire ~GENSYM[pllOutCore][0];
  wire ~GENSYM[globalOutCore][1];
  wire ~GENSYM[lock][2];
  
  SB_PLL40_CORE #(
    .DIVF                           ( ~ARG[1]  ),
    .DIVQ                           ( ~ARG[2]  ),
    .DIVR                           ( ~ARG[3]  ),
    .FEEDBACK_PATH                  ( ~ARG[4]  ),
    .FILTER_RANGE                   ( ~ARG[5]  ),
    .PLLOUT_SELECT                  ( ~ARG[6]  ),
    .DELAY_ADJUSTMENT_MODE_FEEDBACK ( ~ARG[7]  ),
    .DELAY_ADJUSTMENT_MODE_RELATIVE ( ~ARG[8]  ),
    .FDA_FEEDBACK                   ( ~ARG[9]  ),
    .FDA_RELATIVE                   ( ~ARG[10] ),
    .ENABLE_ICEGATE                 ( ~ARG[11] )
  ) ~GENSYM[sb_pll40_core_inst][3] (
    .REFERENCECLK                   ( ~ARG[12] ),
    .DYNAMICDELAY                   ( ~ARG[13] ),
    .RESETB                         ( ~ARG[14] ),
    .BYPASS                         ( ~ARG[15] ),
  
    .PLLOUTCORE                     ( ~SYM[0]  ),
    .PLLOUTGLOBAL                   ( ~SYM[1]  ),
    .LOCK                           ( ~SYM[2]  )
  );

  assign ~RESULT = { ~SYM[0], ~SYM[1], ~SYM[2] };
  // SB_PLL40_CORE end"
      }
    }
  ]
  |]) #-}

-- | PLL Core primitive
{-# NOINLINE pllCorePrim #-}
pllCorePrim 
  :: KnownDomain dom'            -- ARG[0]
  => BitVector 7                 -- ^ divf
  -> BitVector 3                 -- ^ divq
  -> BitVector 4                 -- ^ divr
  -> String                      -- ^ feedbackPath
  -> BitVector 3                 -- ^ filterRange
  -> String                      -- ^ pllOutSelect
  -> String                      -- ^ delayAdjustmentModeFeedback
  -> String                      -- ^ delayAdjustmentModeRelative
  -> BitVector 4                 -- ^ fdaFeedback
  -> BitVector 4                 -- ^ fdaRelative
  -> Bit                         -- ^ enableIceGate
  -> Clock dom                   -- ^ referenceClk
  -> Signal dom (BitVector 8)    -- ^ dynamicDelay
  -> Signal dom Bit              -- ^ resetb
  -> Signal dom Bit              -- ^ bypass
  -> ( Clock dom'                -- pllOutCore
     , Clock dom'                -- globalOutCore
     , Signal dom' Bool          -- lock
     ) -- ^ (pllOutCore, globalOutCore, lock)
pllCorePrim !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ = (clockGen, clockGen, pure True)
