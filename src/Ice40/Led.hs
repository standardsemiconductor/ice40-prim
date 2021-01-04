module Ice40.Led where

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
                   )
ledPrim !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ = (pure 0, pure 0, pure 0, pure False)
