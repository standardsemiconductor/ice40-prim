module Ice40.IO where

import Clash.Prelude
import Clash.Annotations.Primitive
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)

{-# ANN ioPrim hasBlackBox #-}
{-# ANN ioPrim (InlinePrimitive [Verilog] $ unindent [i|
  [  { "BlackBox" :
       { "name" : "Ice40.IO.ioPrim"
       , "kind" : "Declaration"
       , "type" :
  "ioPrim
    :: BitVector 6    -- ARG[0]  pinType
    -> Bit            -- ARG[1]  pullup
    -> Bit            -- ARG[2]  negTrigger
    -> String         -- ARG[3]  ioStandard
    -> Signal dom Bit -- ARG[4]  latchInputValue
    -> Signal dom Bit -- ARG[5]  clockEnable
    -> Clock domIn    -- ARG[6]  inputClk
    -> Clock domOut   -- ARG[7]  outputClk
    -> Signal dom Bit -- ARG[8]  outputEnable
    -> Signal dom Bit -- ARG[9]  dOut0
    -> Signal dom Bit -- ARG[10] dOut1
    -> Unbundled dom
         ( Bit -- packagePin
         , Bit -- dIn0
         , Bit -- dIn1
         )"
       , "template" :
  "//SB_IO begin
  wire ~GENSYM[package_pin][0];
  wire ~GENSYM[d_in_0][1];
  wire ~GENSYM[d_in_1][2];

  SB_IO #(
    .PIN_TYPE         ( ~ARG[0]  ),
    .PULLUP           ( ~ARG[1]  ),
    .NEG_TRIGGER      ( ~ARG[2]  ),
    .IO_STANDARD      ( ~ARG[3]  )
  ) ~GENSYM[sb_io_inst][3] (
    .PACKAGE_PIN      ( ~SYM[0]  ),
    .LATCH_INPUT_VALUE( ~ARG[4]  ),
    .CLOCK_ENABLE     ( ~ARG[5]  ),
    .INPUT_CLK        ( ~ARG[6]  ),
    .OUTPUT_CLK       ( ~ARG[7]  ),
    .OUTPUT_ENABLE    ( ~ARG[8]  ),
    .D_OUT_0          ( ~ARG[9]  ),
    .D_OUT_1          ( ~ARG[10] ),
    .D_IN_0           ( ~SYM[1]  ),
    .D_IN_1           ( ~SYM[2]  )
  );

  assign ~RESULT = { ~SYM[0], ~SYM[1], ~SYM[2] };
  //SB_IO end"
       }
     }
  ]
  |]) #-}

{-# NOINLINE ioPrim #-}
ioPrim
  :: BitVector 6    -- ARG[0]  pinType
  -> Bit            -- ARG[1]  pullup
  -> Bit            -- ARG[2]  negTrigger
  -> String         -- ARG[3]  ioStandard
  -> Signal dom Bit -- ARG[4]  latchInputValue
  -> Signal dom Bit -- ARG[5]  clockEnable
  -> Clock domIn    -- ARG[6]  inputClk
  -> Clock domOut   -- ARG[7]  outputClk
  -> Signal dom Bit -- ARG[8]  outputEnable
  -> Signal dom Bit -- ARG[9]  dOut0
  -> Signal dom Bit -- ARG[10] dOut1
  -> Unbundled dom
       ( Bit -- packagePin
       , Bit -- dIn0
       , Bit -- dIn1
       )
ioPrim !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ = (pure 0, pure 0, pure 0)
