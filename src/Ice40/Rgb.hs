module Ice40.Rgb where

import Clash.Prelude
import Clash.Annotations.Primitive
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)

{-# ANN rgbPrim (InlinePrimitive [Verilog] $ unindent [i|
  [ { "BlackBox" :
      { "name" : "Ice40.Rgb.rgbPrim"
      , "kind" : "Declaration"
      , "type" :
  "rgbPrim
  :: String           -- currentMode ARG[0]
  -> String           -- rgb0Current ARG[1]
  -> String           -- rgb1Current ARG[2]
  -> String           -- rgb2Current ARG[3]
  -> Signal dom Bit   -- curren      ARG[4]
  -> Signal dom Bit   -- rgbleden    ARG[5]
  -> Signal dom Bit   -- rgb0Pwm     ARG[6]
  -> Signal dom Bit   -- rgb1Pwm     ARG[7]
  -> Signal dom Bit   -- rgb2Pwm     ARG[8]
  -> Signal dom ( Bit -- rgb0
                , Bit -- rgb1
                , Bit -- rgb2
                )"
      , "template" :
  "//SB_RGBA_DRV begin
  wire ~GENSYM[rgb0][0];
  wire ~GENSYM[rgb1][1];
  wire ~GENSYM[rgb2][2];

  SB_RGBA_DRV #(
     .CURRENT_MODE ( ~ARG[0] ),
     .RGB0_CURRENT ( ~ARG[1] ),
     .RGB1_CURRENT ( ~ARG[2] ),
     .RGB2_CURRENT ( ~ARG[3] )
  ) ~GENSYM[rgba_drv_inst][3] (
     .CURREN       ( ~ARG[4] ),
     .RGBLEDEN     ( ~ARG[5] ),
     .RGB0PWM      ( ~ARG[6] ),
     .RGB1PWM      ( ~ARG[7] ),
     .RGB2PWM      ( ~ARG[8] ),
     .RGB0         ( ~SYM[0] ),
     .RGB1         ( ~SYM[1] ),
     .RGB2         ( ~SYM[2] )
  );
 
  assign ~RESULT = {~SYM[0], ~SYM[1], ~SYM[2]};
  //SB_RGBA_DRV end"
      }
    } 
  ]
  |]) #-}

{-# NOINLINE rgbPrim #-}
rgbPrim
  :: String           -- currentMode
  -> String           -- rgb0Current
  -> String           -- rgb1Current
  -> String           -- rgb2Current
  -> Signal dom Bit   -- curren
  -> Signal dom Bit   -- rgbleden
  -> Signal dom Bit   -- rgb0Pwm
  -> Signal dom Bit   -- rgb1Pwm
  -> Signal dom Bit   -- rgb2Pwm
  -> Signal dom ( Bit -- rgb0
                , Bit -- rgb1
                , Bit -- rgb2
                )
rgbPrim !_ !_ !_ !_ !_ !_ !_ !_ !_ = pure (0, 0, 0)
