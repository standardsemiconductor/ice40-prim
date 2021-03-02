{-|
Module : Ice40.Mac
Description : Ice40 Multiply-Accumulate primitive wrapper
Copyright : (c) David Cox, 2021
License : BSD-3-Clause
Maintainer : standardsemiconductor@gmail.com

MAC primitive wrapper. See `Ice40.Mac.Prim` for the original primitive.
-}
module Ice40.Mac where

import Clash.Prelude
import Ice40.Mac.Prim

-- | MAC inputs
data Input dom = Input
  { ce        :: Signal dom Bit
  , c         :: Signal dom (BitVector 16)
  , a         :: Signal dom (BitVector 16)
  , b         :: Signal dom (BitVector 16)
  , d         :: Signal dom (BitVector 16)
  , irsttop   :: Signal dom Bit
  , irstbot   :: Signal dom Bit
  , orsttop   :: Signal dom Bit
  , orstbot   :: Signal dom Bit
  , ahold     :: Signal dom Bit
  , bhold     :: Signal dom Bit
  , chold     :: Signal dom Bit
  , dhold     :: Signal dom Bit
  , oholdtop  :: Signal dom Bit
  , oholdbot  :: Signal dom Bit
  , addsubtop :: Signal dom Bit
  , addsubbot :: Signal dom Bit
  , oloadtop  :: Signal dom Bit
  , oloadbot  :: Signal dom Bit
  , accumci   :: Signal dom Bit
  , signextin :: Signal dom Bit
  , ci        :: Signal dom Bit
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
  { negTrigger :: Bit
  , aReg :: Bit
  , bReg :: Bit
  , cReg :: Bit
  , dReg :: Bit
  , top8x8MultReg :: Bit
  , bot8x8MultReg :: Bit
  , pipeline16x16MultReg1 :: Bit
  , pipeline16x16MultReg2 :: Bit
  , topOutputSelect      :: BitVector 2
  , topAddSubLowerInput  :: BitVector 2
  , topAddSubUpperInput  :: Bit
  , topAddSubCarrySelect :: BitVector 2
  , botOutputSelect      :: BitVector 2
  , botAddSubLowerInput  :: BitVector 2
  , botAddSubUpperInput  :: Bit
  , botAddSubCarrySelect :: BitVector 2
  , mode8x8 :: Bit
  , aSigned :: Bit
  , bSigned :: Bit
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

