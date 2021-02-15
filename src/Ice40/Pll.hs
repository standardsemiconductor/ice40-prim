{-|
Module      : Ice40.Pll
Description : Ice40 PLL hard IP primitive
Copyright   : (c) David Cox, 2021
License     : BSD 3-Clause
Maintainer  : standardsemiconductor@gmail.com
PLL hard IP primitive from Lattice Ice Technology Library https://github.com/standardsemiconductor/VELDT-info/blob/master/SBTICETechnologyLibrary201708.pdf
-}
module Ice40.Pll ( pll ) where

import Clash.Prelude
import Clash.Annotations.Primitive
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)

{-# ANN pllPrim (InlinePrimitive [Verilog] $ unindent [i|
  [  { "BlackBox" :
       { "name" : "Ice40.Pll.pllPrim"
       , "kind" : "Declaration"
       , "type" :
  "pllPrim
    :: String          -- ARG[0]  feedbackpath
    -> String          -- ARG[1]  delayAdjustmentModeFeedback
    -> BitVector 4     -- ARG[2]  fdaFeedback
    -> String          -- ARG[3]  delayAdjustmentModeRelative
    -> BitVector 4     -- ARG[4]  fdaRelative
    -> Clock dom       -- ARG[1]  referenceClk
    -> Signal dom Bool -- ARG[2]  sbrwi
    -> Signal dom Bool -- ARG[3]  sbstbi
    -> Signal dom Bit  -- ARG[4]  sbadri7
    -> Signal dom Bit  -- ARG[5]  sbadri6
    -> Signal dom Bit  -- ARG[6]  sbadri5
    -> Signal dom Bit  -- ARG[7]  sbadri4
    -> Signal dom Bit  -- ARG[8]  sbadri3
    -> Signal dom Bit  -- ARG[9]  sbadri2
    -> Signal dom Bit  -- ARG[10] sbadri1
    -> Signal dom Bit  -- ARG[11] sbadri0
    -> Signal dom Bit  -- ARG[12] sbdati7
    -> Unbundled dom 
         ( BitVector 8 -- sbdato
         , Bool        -- sbacko
         , Bit         -- spiirq
         , Bit         -- spiwkup
         , Bit         -- wo
         , Bit         -- woe
         , Bit         -- bo
         , Bit         -- boe
         , Bit         -- wcko
         , Bit         -- wckoe
         , BitVector 4 -- bcsno
         , BitVector 4 -- bcsnoe
         )"
        , "template" :
  "//SB_PLL40_CORE begin
  wire ~GENSYM[lock][0];
  wire ~GENSYM[plloutglobal][1];
  wire ~GENSYM[plloutcore][2];

  SB_PLL40_CORE #(
    .FEEDBACK_PATH                  ( ~ARG[0] ),
    .DELAY_ADJUSTMENT_MODE_FEEDBACK ( ~ARG[1] ),
    .FDA_FEEDBACK                   ( ~ARG[2] ),
    .DELAY_ADJUSTMENT_MODE_RELATIVE ( ~ARG[3] ),
    .FDA_RELATIVE                   ( ~ARG[4] ),
    .SHIFTREG_DIV_MODE              ( ~ARG[5] ),
    .PLLOUT_SELECT                  ( ~ARG[6] ),
    .DIVR                           ( ~ARG[7] ),
    .DIVF                           ( ~ARG[8] ),
    .DIVQ                           ( ~ARG[9] )
  ) ~GENSYM[sb_spi_inst][3] (
    .REFERENCECLK    ( ~ARG[8]  ),
    .RESETB          ( ~ARG[9]  ),
    .BYPASS          ( ~ARG[10] ),
    .EXTFEEDBACK     ( ~ARG[11] ),
    .DYNAMICDELAY    ( ~ARG[12] ),
    .LATCHINPUTVALUE ( ~ARG[13] ),
    .SCLK            ( ~ARG[14] ),
    .SDI             ( ~ARG[15] ),
    .SDO             ( ~ARG[16] ),

    .LOCK         ( ~SYM[0]  ),
    .PLLOUTGLOBAL ( ~SYM[1]  ),
    .PLLOUTCORE   ( ~SYM[2]  )
  );

  assign ~RESULT = { ~SYM[0]  // lock
                   , ~SYM[1]  // plloutglobal
                   , ~SYM[2]  // plloutcore
                   };
  // SB_PLL40_CORE end"
       }
     }
  ]
  |]) #-}
