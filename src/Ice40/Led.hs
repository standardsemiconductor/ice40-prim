module Ice40.Led
  ( led
  , Instr(..)
  , toInstrM
  ) where

import Clash.Prelude
import Clash.Annotations.Primitive
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)
import Data.Maybe (isJust, fromMaybe)
import Data.Function ((&))
import qualified Semi.Ice40.Rgb as R

{-# ANN ledPrim (InlinePrimitive [Verilog] $ unindent [i|
  [  { "BlackBox" :
       { "name" : "Semi.Ice40.Led.ledPrim"
       , "kind" : "Declaration"
       , "type" :
  "ledPrim
  :: Clock dom       -- ARG[0] leddclk
  -> Signal dom Bit  -- ARG[1] leddcs
  -> Signal dom Bit  -- ARG[2] ledddat7
  -> Signal dom Bit  -- ARG[3] ledddat6
  -> Signal dom Bit  -- ARG[4] ledddat5
  -> Signal dom Bit  -- ARG[5] ledddat4
  -> Signal dom Bit  -- ARG[6] ledddat3
  -> Signal dom Bit  -- ARG[7] ledddat2
  -> Signal dom Bit  -- ARG[8] ledddat1
  -> Signal dom Bit  -- ARG[9] ledddat0
  -> Signal dom Bit  -- ARG[10] leddaddr3
  -> Signal dom Bit  -- ARG[11] leddaddr2
  -> Signal dom Bit  -- ARG[12] leddaddr1
  -> Signal dom Bit  -- ARG[13] leddaddr0
  -> Signal dom Bool -- ARG[14] leddden
  -> Signal dom Bool -- ARG[15] leddexe
  -> Unbundled dom (Bit, Bit, Bit, Bool) -- (pwm0, pwm1, pwm2, leddon)"
      , "template" :
  "//SB_LEDDA_IP begin
  wire ~GENSYM[pwm0][0];
  wire ~GENSYM[pwm1][1];
  wire ~GENSYM[pwm2][2];
  wire ~GENSYM[leddon][3];

  SB_LEDDA_IP SB_LEDDA_IP_INST (
    .LEDDCLK(~ARG[0]),
    .LEDDCS(~ARG[1]),
    .LEDDDAT7(~ARG[2]),
    .LEDDDAT6(~ARG[3]),
    .LEDDDAT5(~ARG[4]),
    .LEDDDAT4(~ARG[5]),
    .LEDDDAT3(~ARG[6]),
    .LEDDDAT2(~ARG[7]),
    .LEDDDAT1(~ARG[8]),
    .LEDDDAT0(~ARG[9]),
    .LEDDADDR3(~ARG[10]),
    .LEDDADDR2(~ARG[11]),
    .LEDDADDR1(~ARG[12]),
    .LEDDADDR0(~ARG[13]),
    .LEDDDEN(~ARG[14]),
    .LEDDEXE(~ARG[15]),
    .PWMOUT0(~SYM[0]),
    .PWMOUT1(~SYM[1]),
    .PWMOUT2(~SYM[2]),
    .LEDDON(~SYM[3])
  );

  assign ~RESULT = {~SYM[0], ~SYM[1], ~SYM[2], ~SYM[3]};
  //SB_LEDDA_IP end"
      }
    }
  ]
  |]) #-}

{-# NOINLINE ledPrim #-}
ledPrim
  :: Clock dom       -- ARG[0] leddclk
  -> Signal dom Bit  -- ARG[1] leddcs
  -> Signal dom Bit  -- ARG[2] ledddat7
  -> Signal dom Bit  -- ARG[3] ledddat6
  -> Signal dom Bit  -- ARG[4] ledddat5
  -> Signal dom Bit  -- ARG[5] ledddat4
  -> Signal dom Bit  -- ARG[6] ledddat3
  -> Signal dom Bit  -- ARG[7] ledddat2
  -> Signal dom Bit  -- ARG[8] ledddat1
  -> Signal dom Bit  -- ARG[9] ledddat0
  -> Signal dom Bit  -- ARG[10] leddaddr3
  -> Signal dom Bit  -- ARG[11] leddaddr2
  -> Signal dom Bit  -- ARG[12] leddaddr1
  -> Signal dom Bit  -- ARG[13] leddaddr0
  -> Signal dom Bool -- ARG[14] leddden
  -> Signal dom Bool -- ARG[15] leddexe
  -> Unbundled dom (Bit, Bit, Bit, Bool) -- (pwm0, pwm1, pwm2, leddon)"
ledPrim !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ = (pure 0, pure 0, pure 0, pure False)

type Byte = BitVector 8
data Instr = Cr0  Byte
           | Br   Byte
           | Onr  Byte
           | Ofr  Byte
           | Bcrr Byte
           | Bcfr Byte
           | Pwrr Byte
           | Pwrg Byte
           | Pwrb Byte
  deriving (NFDataX, Generic)

fromInstr :: Instr -> (BitVector 4, Byte)
fromInstr = \case
  Cr0  b -> (0b1000, b)
  Br   b -> (0b1001, b)
  Onr  b -> (0b1010, b)
  Ofr  b -> (0b1011, b)
  Bcrr b -> (0b0101, b)
  Bcfr b -> (0b0110, b)
  Pwrr b -> (0b0001, b)
  Pwrg b -> (0b0010, b)
  Pwrb b -> (0b0011, b)

toInstrM :: (BitVector 4, Byte) -> Maybe Instr
toInstrM (addr, b) = b & case addr of
  $(bitPattern "1000") -> Just . Cr0
  $(bitPattern "1001") -> Just . Br
  $(bitPattern "1010") -> Just . Onr
  $(bitPattern "1011") -> Just . Ofr
  $(bitPattern "0101") -> Just . Bcrr
  $(bitPattern "0110") -> Just . Bcfr
  $(bitPattern "0001") -> Just . Pwrr
  $(bitPattern "0010") -> Just . Pwrg
  $(bitPattern "0011") -> Just . Pwrb
  _ -> const Nothing

led
  :: Clock dom
  -> Signal dom Bit
  -> Signal dom (Maybe Instr)
  -> Signal dom Bool
  -> Unbundled dom (R.Rgb, Bool)
led clk cs instrM exe = (bundle (pwmR, pwmG, pwmB), ledOn)
  where
    en = isJust <$> instrM
    (addr, dat) = unbundle $ fromInstr . fromMaybe (Cr0 0) <$> instrM
    (pwmR, pwmG, pwmB, ledOn) = ledPrim clk
                                        cs
                                        (bitAt 7 <$> dat)
                                        (bitAt 6 <$> dat)
                                        (bitAt 5 <$> dat)
                                        (bitAt 4 <$> dat)
                                        (bitAt 3 <$> dat)
                                        (bitAt 2 <$> dat)
                                        (bitAt 1 <$> dat)
                                        (bitAt 0 <$> dat)
                                        (bitAt 3 <$> addr)
                                        (bitAt 2 <$> addr)
                                        (bitAt 1 <$> addr)
                                        (bitAt 0 <$> addr)
                                        en
                                        exe

bitAt :: KnownNat n => Index n -> BitVector n -> Bit
bitAt = flip (!)
