{-|
Module      : Ice40.Osc
Description : Ice40 oscillator hard IP primitives
Copyright   : (c) David Cox, 2021-2022
License     : BSD 3-Clause
Maintainer  : standardsemiconductor@gmail.com

LFOSC and HFOSC hard IP primitive from [Lattice Ice Technology Library](https://github.com/standardsemiconductor/VELDT-info/blob/master/SBTICETechnologyLibrary201708.pdf)
-}
module Ice40.Osc
  ( lf10kHz
  , hf48Mhz
  , hf24Mhz
  , hf12Mhz
  , hf6Mhz
  ) where

import Clash.Prelude
import Clash.Signal.Internal
import Clash.Annotations.Primitive
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)
import Ice40.Clock

------------------------------
-- Low Frequency Oscillator --
------------------------------

{-# ANN lf10kHz (InlinePrimitive [Verilog] $ unindent [i|
  [  { "BlackBox" :
       { "name" : "Ice40.Osc.lf10kHz"
       , "kind" : "Declaration"
       , "type" :
  "lf10kHz
  :: Signal dom Bool -- ARG[0] clkLfPu
  -> Signal dom Bool -- ARG[1] clkLfEn
  -> Clock Lattice10kHz"
       , "template" :
  "//SB_LFOSC begin
  SB_LFOSC ~GENSYM[sb_lfosc_inst][0] (   
  .CLKLFEN (~ARG[0]),
  .CLKLFPU (~ARG[1]), 
  .CLKLF   (~RESULT)
  );
  //SB_LFOSC end"
      }
    }
  ]
  |]) #-}

-- | Low frequency oscillator 10 kHz
{-# NOINLINE lf10kHz #-}
lf10kHz
  :: Signal dom Bool -- ^ CLKLFPU - Power up the LFOSC circuit. After power up, oscillator output will be stable after 100us. Active High.
  -> Signal dom Bool -- ^ CLKLFEN - Enable the clock output. Enable should be low for the 100us power up period. Active High.
  -> Clock Lattice10kHz -- ^ LF Oscillator output
lf10kHz !_ !_ = Clock SSymbol

-------------------------------
-- High Frequency Oscillator --
-------------------------------
-- | SB_HFOSC primitive generates 48MHz nominal clock frequency within +/-10% variation, with user programmable divider value of 1, 2, 4, and 8. the HFOSC can drive either the global clock network or fabric routes directly based on the clock network selection.

{-# ANN hfPrim (InlinePrimitive [Verilog] $ unindent [i|
  [  { "BlackBox" :
       { "name" : "Ice40.Osc.hfPrim"
       , "kind" : "Declaration"
       , "type" :
  "hfPrim
  :: KnownDomain dom            -- ARG[0]
  => KnownDomain dom'           -- ARG[1]
  => String                     -- ARG[2]  clkhfdiv
  -> Signal dom Bool            -- ARG[3]  clkhfen
  -> Signal dom Bool            -- ARG[4]  clkhfpu
  -> Clock dom'                 -- clkhf"
       , "template" :
  "//SB_HFOSC begin
  SB_HFOSC #( .CLKHF_DIV(~ARG[2]) ) ~GENSYM[sb_hfosc_inst][0] (
    .CLKHFEN (~ARG[3]),
    .CLKHFPU (~ARG[4]),
    .CLKHF   (~RESULT)
  );                                                                                          
  //SB_HFOSC end"
      }
    }
  ]
  |]) #-}

{-# NOINLINE hfPrim #-}
hfPrim
  :: KnownDomain dom            -- ARG[0]
  => KnownDomain dom'           -- ARG[1]
  => String                     -- ARG[2]  clkhfdiv
  -> Signal dom Bool            -- ARG[3]  clkhfen
  -> Signal dom Bool            -- ARG[4]  clkhfpu
  -> Clock dom'                 -- clkhf"
hfPrim !_ !_ !_ = Clock SSymbol

-- | High frequency oscillator 48 Mhz
hf48Mhz
  :: KnownDomain dom
  => Signal dom Bool -- ^ CLKHFEN Enable the clock output. Enable should be low for the 100us power up period. Active High.
  -> Signal dom Bool -- ^ CLKHFPU Power up the HFOSC circuit. After power up, oscillator output will be stable after 100us. Active High.
  -> Clock Lattice48Mhz -- ^ HF Oscillator output
hf48Mhz = hfPrim "0b00"

-- | High frequency oscillator 24 Mhz
hf24Mhz
  :: KnownDomain dom
  => Signal dom Bool -- ^ CLKHFEN Enable the clock output. Enable should be low for the 100us power up period. Active High.
  -> Signal dom Bool -- ^ CLKHFPU Power up the HFOSC circuit. After power up, oscillator output will be stable after 100us. Active High.
  -> Clock Lattice24Mhz -- ^ HF Oscillator output
hf24Mhz = hfPrim "0b01"

-- | High frequency oscillator 12 Mhz
hf12Mhz
  :: KnownDomain dom
  => Signal dom Bool -- ^ CLKHFEN Enable the clock output. Enable should be low for the 100us power up period. Active High.
  -> Signal dom Bool -- ^ CLKHFPU Power up the HFOSC circuit. After power up, oscillator output will be stable after 100us. Active High.
  -> Clock Lattice12Mhz -- ^ HF Oscillator output
hf12Mhz = hfPrim "0b10"

-- | High frequency oscillator 6 Mhz
hf6Mhz
  :: KnownDomain dom
  => Signal dom Bool -- ^ CLKHFEN Enable the clock output. Enable should be low for the 100us power up period. Active High.
  -> Signal dom Bool -- ^ CLKHFPU Power up the HFOSC circuit. After power up, oscillator output will be stable after 100us. Active High.
  -> Clock Lattice6Mhz -- ^ HF Oscillator output
hf6Mhz = hfPrim "0b11"
  
