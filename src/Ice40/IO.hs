{-|
Module      : Ice40.IO
Description : Ice40 IO hard IP primitives
Copyright   : (c) David Cox, 2021
License     : BSD 3-Clause
Maintainer  : standardsemiconductor@gmail.com

IO hard IP primitive from [Lattice Ice Technology Library](https://github.com/standardsemiconductor/VELDT-info/blob/master/SBTICETechnologyLibrary201708.pdf)
-}
module Ice40.IO where

import Clash.Prelude
import Clash.Annotations.Primitive
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)

{-# ANN ioPrim (InlinePrimitive [Verilog] $ unindent [i|
  [  { "BlackBox" :
       { "name" : "Ice40.IO.ioPrim"
       , "kind" : "Declaration"
       , "type" :
  "ioPrim
    :: BitVector 6         -- ARG[0]  pinType
    -> Bit                 -- ARG[1]  pullup
    -> Bit                 -- ARG[2]  negTrigger
    -> String              -- ARG[3]  ioStandard
    -> Signal domIn Bit    -- ARG[4]  latchInputValue
    -> Signal domEn Bit    -- ARG[5]  clockEnable
    -> Clock domIn         -- ARG[6]  inputClk
    -> Clock domOut        -- ARG[7]  outputClk
    -> Signal domOut Bit   -- ARG[8]  outputEnable
    -> Signal domOut Bit   -- ARG[9]  dOut0
    -> Signal domOut Bit   -- ARG[10] dOut1
    -> ( Signal domPin Bit -- packagePin
       , Signal domIn Bit  -- dIn0
       , Signal domIn Bit  -- dIn1
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

-- | IO primitive, see io for wrapper
{-# NOINLINE ioPrim #-}
ioPrim
  :: BitVector 6         -- ^ pinType
  -> Bit                 -- ^ pullup
  -> Bit                 -- ^ negTrigger
  -> String              -- ^ ioStandard
  -> Signal domIn Bit    -- ^ latchInputValue
  -> Signal domEn Bit    -- ^ clockEnable
  -> Clock domIn         -- ^ inputClk
  -> Clock domOut        -- ^ outputClk
  -> Signal domOut Bit   -- ^ outputEnable
  -> Signal domOut Bit   -- ^ dOut0
  -> Signal domOut Bit   -- ^ dOut1
  -> ( Signal domPin Bit -- packagePin
     , Signal domIn Bit  -- dIn0
     , Signal domIn Bit  -- dIn1
     ) -- ^ (packagePin, dIn0, dIn1)
ioPrim !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ = (pure 0, pure 0, pure 0)

-- | Input pin configuration parameter
data PinInput = PinInput -- ^ Simple Input pin dIn0
              | PinInputLatch -- ^ Disables Internal data changes on the physical input pin by latching the value
              | PinInputRegistered -- ^ Input data is registered in input cell
              | PinInputRegisteredLatch -- ^ Disables internal data changes on the physical input pin by latching the value on the input register
              | PinInputDDR -- ^ Input DDR data is clocked out on rising and falling clock edges. Use the dIn0 and dIn1 pins for DDR operation
  deriving stock (Generic, Show, Read, Eq)
  deriving anyclass NFDataX

-- | Convert `PinInput` to underlying `BitVector`
fromPinInput :: PinInput -> BitVector 2
fromPinInput = \case
  PinInput                -> 0b01
  PinInputLatch           -> 0b11
  PinInputRegistered      -> 0b00
  PinInputRegisteredLatch -> 0b10
  PinInputDDR             -> 0b00

-- | Output pin configuration parameter
data PinOutput = PinNoOutput -- ^Disables the output function
               | PinOutput -- ^ Simple output pin (no enable)
               | PinOutputTristate -- ^ The output pin may be tristated using the enable
               | PinOutputEnableRegistered -- ^ The output pin may be tristated using a registered enable signal
               | PinOutputRegistered -- ^ Output registered (no enable)
               | PinOutputRegisteredEnable -- ^ Output registered with enable (the enable is not registered)
               | PinOutputRegisteredEnableRegistered -- ^ Output registered and enable registered
               | PinOutputDDR -- ^ Output DDR data is clocked out on rising and falling clock edges
               | PinOutputDDREnable -- ^ Output data is clocked out on rising and falling clock edges
               | PinOutputDDREnableRegistered -- ^ Output DDR data with registered enable signal
               | PinOutputRegisteredInverted -- ^ Output registered signal is inverted
               | PinOutputRegisteredEnableInverted -- ^ Output signal is registered and inverted (no enable function)
               | PinOutputRegisteredEnableRegisteredInverted -- ^ Output signal is registered and inverted, the enable/tristate control is registered
  deriving stock (Generic, Show, Read, Eq)
  deriving anyclass NFDataX

-- | Convert `PinOutput` to underlying `BitVector`
fromPinOutput :: PinOutput -> BitVector 4
fromPinOutput = \case
  PinNoOutput                                 -> 0b0000
  PinOutput                                   -> 0b0110
  PinOutputTristate                           -> 0b1010
  PinOutputEnableRegistered                   -> 0b1110
  PinOutputRegistered                         -> 0b0101
  PinOutputRegisteredEnable                   -> 0b1001
  PinOutputRegisteredEnableRegistered         -> 0b1101
  PinOutputDDR                                -> 0b0100
  PinOutputDDREnable                          -> 0b1000
  PinOutputDDREnableRegistered                -> 0b1100
  PinOutputRegisteredInverted                 -> 0b0111
  PinOutputRegisteredEnableInverted           -> 0b1011
  PinOutputRegisteredEnableRegisteredInverted -> 0b1111

-- | Input-Output Standards
data IOStandard = SBLVCMOS
                | SBLVDSINPUT
  deriving (Generic, Show, Read, Eq)

-- | Convert `IOStandard` to underlying `String`
fromIOStandard :: IOStandard -> String
fromIOStandard = \case
  SBLVCMOS    -> "SB_LVCMOS"
  SBLVDSINPUT -> "SB_LVDS_INPUT"

-- | IO primitive
io
  :: PinInput
  -> PinOutput
  -> Bit                 -- ^ pullUp
  -> Bit                 -- ^ negTrigger
  -> IOStandard
  -> Signal domIn Bit    -- ^ latchInputValue
  -> Signal domEn Bit    -- ^ clockEnable
  -> Clock domIn         -- ^ inputClk
  -> Clock domOut        -- ^ outputClk
  -> Signal domOut Bit   -- ^ outputEnable
  -> Signal domOut Bit   -- ^ dOut0
  -> Signal domOut Bit   -- ^ dOut1
  -> ( Signal domPin Bit -- packagePin
     , Signal domIn Bit  -- dIn0
     , Signal domIn Bit  -- dIn1
     ) -- ^ (packagePin, dIn0, dIn1)
io pinInput pinOutput pullUp negTrigger ioStandard
  = ioPrim
      (fromPinOutput pinOutput ++# fromPinInput pinInput)
      pullUp
      negTrigger
      (fromIOStandard ioStandard)

