{-|
Module      : Ice40.Mac
Description : Ice40 Multiply-Accumulate primitive wrapper
Copyright   : (c) David Cox, 2021
License     : BSD-3-Clause
Maintainer  : standardsemiconductor@gmail.com

MAC primitive wrapper. See "Ice40.Mac.Prim" for the original primitive. For more information see [LATTICE ICE Technology Library](https://github.com/standardsemiconductor/VELDT-info/blob/master/SBTICETechnologyLibrary201708.pdf) and [DSP Function Usage Guide](https://github.com/standardsemiconductor/VELDT-info/blob/master/DSPFunctionUsageGuideforICE40Devices.pdf).
-}
module Ice40.Mac 
  ( mac
  , Input(..)
  , defaultInput
  , Parameter(..)
  , defaultParameter
  ) where

import Clash.Prelude
import Ice40.Mac.Prim

-- | MAC inputs
data Input dom = Input
  { ce        :: Signal dom Bit -- ^ clock enable input. applies to all clocked elemets, default = 1
  , c         :: Signal dom (BitVector 16) -- ^ 16-bits data of input c, default = 0
  , a         :: Signal dom (BitVector 16) -- ^ 16-bits data of input a, default = 0
  , b         :: Signal dom (BitVector 16) -- ^ 16-bits data of input b, default = 0
  , d         :: Signal dom (BitVector 16) -- ^ 16-bits data of input d, default = 0
  , irsttop   :: Signal dom Bit -- ^ reset input to registers A and C. Also reset upper 8x8 multiplier output register (8x8 MAC pipeline register) 0 = not reset (default), 1 = reset
  , irstbot   :: Signal dom Bit -- ^ reset input to registers B and D. Also reset lower 8x8 multiplier output register (8x8 MAC pipeline register) and the 16x16 multiplier output register (16x16 MAC pipeline register). 0 = not reset (default), 1 = reset
  , orsttop   :: Signal dom Bit -- ^ reset input to top accumulator register (for adder/subtractor, accumulator, and MAC functions) 0 = not reset (default), 1 = reset
  , orstbot   :: Signal dom Bit -- ^ reset input to bottom accumulator register (for adder/subtractor, accumulator, and MAC functions) 0 = not reset (default), 1 = rest
  , ahold     :: Signal dom Bit -- ^ register A hold input. Control data flow input register A. 0 = load (default), 1 = hold
  , bhold     :: Signal dom Bit -- ^ register B hold input. Control data flow input register B. 0 = load (default), 1 = hold
  , chold     :: Signal dom Bit -- ^ register C hold input. Control data flow input register C. 0 = load (default), 1 = hold
  , dhold     :: Signal dom Bit -- ^ register D hold input. Control data flow input register D. 0 = load (default), 1 = hold
  , oholdtop  :: Signal dom Bit -- ^ top accumulator output register hold input. control data flow into the register. 0 = load (default), 1 = hold
  , oholdbot  :: Signal dom Bit -- ^ bottom accumulator output register hold input. control data flow into the register. 0 = load (default), 1 = hold
  , addsubtop :: Signal dom Bit -- ^ add/subtract control input to top accumulator. 0 = add (default), 1 = subtract
  , addsubbot :: Signal dom Bit -- ^ add/subtract control input to bottom accumulator. 0 = add (default), 1 = subtract
  , oloadtop  :: Signal dom Bit -- ^ load control input to top accumulator register (initialize on MAC function). 0 = not load (default), 1 = load data from register/input C
  , oloadbot  :: Signal dom Bit -- ^ load control input to bottom accumulator register (initialize on MAC function). 0 = not load (default), 1 = load data from register/input D
  , accumci   :: Signal dom Bit -- ^ cascaded accumulator carry input from previous DSP block, default = 0
  , signextin :: Signal dom Bit -- ^ sign extension input from previous DSP block, default = 0
  , ci        :: Signal dom Bit -- ^ cascaded add/sub carry input from previous DSP block, default = 0
  }

-- | default MAC inputs
defaultInput :: Input dom
defaultInput = Input
  { ce        = 1
  , c         = 0
  , a         = 0
  , b         = 0
  , d         = 0
  , irsttop   = 0
  , irstbot   = 0
  , orsttop   = 0
  , orstbot   = 0
  , ahold     = 0
  , bhold     = 0
  , chold     = 0
  , dhold     = 0
  , oholdtop  = 0
  , oholdbot  = 0
  , addsubtop = 0
  , addsubbot = 0
  , oloadtop  = 0
  , oloadbot  = 0
  , accumci   = 0
  , signextin = 0
  , ci        = 0
  }

-- | MAC parameters
data Parameter = Parameter
  { negTrigger :: Bit -- ^ input clock polarity, 0 = rising edge (default), 1 = falling edge
  , aReg :: Bit -- ^ input A register control, 0 = not registered (default), 1 = registered
  , bReg :: Bit -- ^ input B register control, 0 = not registered (default), 1 = registered
  , cReg :: Bit -- ^ input C register control, 0 = not registered (default), 1 = registered
  , dReg :: Bit -- ^ input D register control, 0 = not registered (default), 1 = registered
  , top8x8MultReg :: Bit -- ^ top 8x8 multiplier output register control (pipeline register for MAC). 0 = not registered (default), 1 = registered
  , bot8x8MultReg :: Bit -- ^ bottom 8x8 multiplier output register control (pipeline register for MAC). 0 = not registered (default), 1 = registered
  , pipeline16x16MultReg1 :: Bit -- ^ 16x16 multiplier pipeline register control. 0 = not registered (default), 1 = registered
  , pipeline16x16MultReg2 :: Bit -- ^ 16x16 multiplier output register control (pipeline register for MAC). 0 = not registered (default), 1 = registered
  , topOutputSelect      :: BitVector 2 -- ^ top output select. 00 = adder-subtractor not registered (default), 01 = adder-subtractor registered, 10 = 8x8 multiplier, 11 = 16x16 multiplier
  , topAddSubLowerInput  :: BitVector 2 -- ^ input X of upper adder-subtractor. 00 = input A (default), 01 = 8x8 multiplier output at top, 10 = 16x16 multiplier upper 16-bit outputs, 11 = sign extension from Z15 (lower adder-subtractor input)
  , topAddSubUpperInput  :: Bit -- ^ input W of upper adder-subtractor. 0 = output of adder-subtractor register (accumulation function) (default), 1 = input C
  , topAddSubCarrySelect :: BitVector 2 -- ^ carry input select top adder-subtractor, 00 = constant 0 (default), 01 = constant 1, 10 = cascade ACCUMOUT from lower adder-subtractor, 11 = cascade CO from lower adder-subtractor
  , botOutputSelect      :: BitVector 2 -- ^ bottom output select. 00 = adder-subtractor not registered (default), 01 = adder-subtractor registered, 10 = 8x8 multiplier, 16x16 multiplier
  , botAddSubLowerInput  :: BitVector 2 -- ^ input Z of upper adder-subtractor. 00 = input B (default), 01 = 8x8 multiplier output at top, 10 = 16x16 multiplier upper 16-bit outputs, 11 = sign extension from SIGNEXTIN
  , botAddSubUpperInput  :: Bit -- ^ input Y of upper adder-subtractor. 0 = output of adder-subtractor output register (accumulation function) (default), 1 = input D
  , botAddSubCarrySelect :: BitVector 2 -- ^ carry input select bottom adder-subtractor. 00 = constant 0 (default), 01 = constant 1, 10 = cascade ACCUMOUT from lower DSP block, 11 = cascade CO from lower DSP block
  , mode8x8 :: Bit -- ^ select 8x8 multiplier mode (power saving. 0 = not selected (default), 1 = selected
  , aSigned :: Bit -- ^ input A sign. 0 = input A is unsigned (default), 1 = input A is signed
  , bSigned :: Bit -- ^ input B sign. 0 = input B is unsigned (default), 1 = input B is signed
  }

-- | default MAC parameters
defaultParameter :: Parameter
defaultParameter = Parameter
  { negTrigger = 0
  , aReg = 0
  , bReg = 0
  , cReg = 0
  , dReg = 0
  , top8x8MultReg = 0
  , bot8x8MultReg = 0
  , pipeline16x16MultReg1 = 0
  , pipeline16x16MultReg2 = 0
  , topOutputSelect = 0
  , topAddSubLowerInput = 0
  , topAddSubUpperInput = 0
  , topAddSubCarrySelect = 0
  , botOutputSelect = 0
  , botAddSubLowerInput = 0
  , botAddSubUpperInput = 0
  , botAddSubCarrySelect = 0
  , mode8x8 = 0
  , aSigned = 0
  , bSigned = 0
  }

-- | MAC primitive wrapper
mac
  :: HiddenClock dom
  => Parameter
  -> Input dom
  -> ( Signal dom (BitVector 32)
     , Signal dom Bit
     , Signal dom Bit
     , Signal dom Bit
     )
mac Parameter{..} Input{..}
  = macPrim negTrigger
            aReg
            bReg
            cReg
            dReg
            top8x8MultReg
            bot8x8MultReg
            pipeline16x16MultReg1
            pipeline16x16MultReg2
            topOutputSelect
            topAddSubLowerInput
            topAddSubUpperInput
            topAddSubCarrySelect
            botOutputSelect
            botAddSubLowerInput
            botAddSubUpperInput
            botAddSubCarrySelect
            mode8x8
            aSigned
            bSigned
            hasClock
            ce
            c
            a
            b
            d
            irsttop
            irstbot
            orsttop
            orstbot
            ahold
            bhold
            chold
            dhold
            oholdtop
            oholdbot
            addsubtop
            addsubbot
            oloadtop
            oloadbot
            accumci
            signextin
            ci

