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

{-# ANN lfPrim (InlinePrimitive [Verilog] $ unindent [i|
  [  { "BlackBox" :
       { "name" : "Ice40.Osc.lfPrim"
       , "kind" : "Declaration"
       , "type" :
  "lfPrim :: Clock Lattice10kHz"
       , "template" :
  "//SB_LFOSC begin
  SB_LFOSC sb_lfosc_inst (   
  .CLKLFEN (1),
  .CLKLFPU (1), 
  .CLKLF   (~RESULT)
  );
  //SB_LFOSC end"
      }
    }
  ]
  |]) #-}

{-# ANN lfPrim hasBlackBox #-}
{-# NOINLINE lfPrim #-}
lfPrim :: Clock Lattice10kHz
lfPrim = Clock SSymbol

{-# NOINLINE lf10kHz #-}
lf10kHz :: Clock Lattice10kHz
lf10kHz = lfPrim

-------------------------------
-- High Frequency Oscillator --
-------------------------------

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
  SB_HFOSC #( .CLKHF_DIV(~ARG[2]) ) sb_hfosc_inst (
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

hf48Mhz
  :: KnownDomain dom
  => Signal dom Bool
  -> Signal dom Bool
  -> Clock Lattice48Mhz
hf48Mhz = hfPrim "0b00"

hf24Mhz
  :: KnownDomain dom
  => Signal dom Bool
  -> Signal dom Bool
  -> Clock Lattice24Mhz
hf24Mhz = hfPrim "0b01"

hf12Mhz
  :: KnownDomain dom
  => Signal dom Bool
  -> Signal dom Bool
  -> Clock Lattice12Mhz
hf12Mhz = hfPrim "0b10"

hf6Mhz
  :: KnownDomain dom
  => Signal dom Bool
  -> Signal dom Bool
  -> Clock Lattice6Mhz
hf6Mhz = hfPrim "0b11"
  
