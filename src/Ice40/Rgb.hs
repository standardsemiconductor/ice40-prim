{-|
Module      : Ice40.Rgb
Description : Ice40 RGB hard IP primitive
Copyright   : (c) David Cox, 2021
License     : BSD 3-Clause
Maintainer  : standardsemiconductor@gmail.com

RGB hard IP primitive from [Lattice Ice Technology Library](https://github.com/standardsemiconductor/VELDT-info/blob/master/SBTICETechnologyLibrary201708.pdf). See [iCE40 LED Driver Usage Guide](https://github.com/standardsemiconductor/VELDT-info/blob/master/ICE40LEDDriverUsageGuide.pdf) for more information.
-}
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

-- | SB_RGBA_DRV primitive is the RGB LED drive module which contains 3 
--   dedicated open drain I/O pins for RGB LED outputs. Each of the RGB LED 
--   output is bonded out together with an SB_IO_OD primitive to 
--   the package pin. User can either use SB_RGB_DRV primitive or the 
--   SB_IO_OD primitive to drive the package pin, but not both.
-- 
--   The primitive allows configuration of each of the 3 RGB LED 
--   outputs individually. When the RGBx_CURRENT parameter of RGBx output is 
--   set to "0b000000", then SB_IO_OD can be used to drive 
--   the package pin. 
--   
--   +-----------------------+-------------------+-------------------+
--   | RGB Current Parameter | Full Mode Current | Half Mode Current |
--   +=======================+===================+===================+
--   | "0b000001"            | 4mA               | 2mA               |
--   +-----------------------+-------------------+-------------------+
--   | "0b000011"            | 8mA               | 4mA               |
--   +-----------------------+-------------------+-------------------+
--   | "0b000111"            | 12mA              | 6mA               |
--   +-----------------------+-------------------+-------------------+
--   | "0b001111"            | 16mA              | 8mA               |
--   +-----------------------+-------------------+-------------------+
--   | "0b011111"            | 20mA              | 10mA              |
--   +-----------------------+-------------------+-------------------+
--   | "0b111111"            | 24mA              | 12mA              |
--   +-----------------------+-------------------+-------------------+
{-# NOINLINE rgbPrim #-}
rgbPrim
  :: String           -- ^ currentMode - Parameter values: "0b0" = Full Current Mode (Default), "0b1" = Half Current Mode.
  -> String           -- ^ rgb0Current
  -> String           -- ^ rgb1Current
  -> String           -- ^ rgb2Current
  -> Signal dom Bit   -- ^ curren - enable the mixed signal control block to supply reference current to the IR drivers. When it is not enabled (CURREN=0), no current is supplied, and the IR drivers are powered down. Enabling the mixed signal control block takes 100us to reach a stable reference current value.
  -> Signal dom Bit   -- ^ rgbleden - enable the SB_RGB_DRV primitive. Active High.
  -> Signal dom Bit   -- ^ rgb0Pwm - input data to drive RGB0 LED pin. This input is usually driven from the SB_LEDD_IP.
  -> Signal dom Bit   -- ^ rgb1Pwm - input data to drive RGB1 LED pin. This input is usually driven from the SB_LEDD_IP.
  -> Signal dom Bit   -- ^ rgb2Pwm - input data to drive RGB2 LED pin. This input is usually driven from teh SB_LEDD_IP.
  -> Signal dom ( Bit
                , Bit
                , Bit
                ) -- ^ (RGB0 LED output, RGB1 LED output, RGB2 LED output)
rgbPrim !_ !_ !_ !_ !_ !_ !_ !_ !_ = pure (0, 0, 0)
