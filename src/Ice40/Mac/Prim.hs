{-|
Module      : Ice40.Mac.Prim
Description : Ice40 Multiply-Accumulate (DSP) hard IP primitive
Copyright   : (c) David Cox, 2021-2024
License     : BSD 3-Clause
Maintainer  : standardsemiconductor@gmail.com

MAC hard IP primitive from [Lattice Ice Technology Library](https://github.com/standardsemiconductor/VELDT-info/blob/master/SBTICETechnologyLibrary201708.pdf). See [DSP Function Usage Guide](https://github.com/standardsemiconductor/VELDT-info/blob/master/DSPFunctionUsageGuideforICE40Devices.pdf) for more information.
-}
module Ice40.Mac.Prim ( macPrim ) where

import Clash.Prelude
import Clash.Annotations.Primitive
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)

{-# ANN macPrim (InlinePrimitive [Verilog] $ unindent [i|
  [  { "BlackBox" :
       { "name" : "Ice40.Mac.Prim.macPrim"
       , "kind" : "Declaration"
       , "type" :
  "macPrim
  :: Bit                         -- ARG[0]  negTrigger
  -> Bit                         -- ARG[1]  aReg
  -> Bit                         -- ARG[2]  bReg
  -> Bit                         -- ARG[3]  cReg
  -> Bit                         -- ARG[4]  dReg
  -> Bit                         -- ARG[5]  top8x8MultReg
  -> Bit                         -- ARG[6]  bot8x8MultReg
  -> Bit                         -- ARG[7]  pipeline16x16MultReg1
  -> Bit                         -- ARG[8]  pipeline16x16MultReg2
  -> BitVector 2                 -- ARG[9]  topOutputSelect
  -> BitVector 2                 -- ARG[10] topAddSubLowerInput
  -> Bit                         -- ARG[11] topAddSubUpperInput
  -> BitVector 2                 -- ARG[12] topAddSubCarrySelect
  -> BitVector 2                 -- ARG[13] botOutputSelect
  -> BitVector 2                 -- ARG[14] botAddSubLowerInput
  -> Bit                         -- ARG[15] botAddSubUpperInput
  -> BitVector 2                 -- ARG[16] botAddSubCarrySelect
  -> Bit                         -- ARG[17] mode8x8
  -> Bit                         -- ARG[18] aSigned
  -> Bit                         -- ARG[19] bSigned
  -> Clock dom                   -- ARG[20] clk
  -> Signal dom Bit              -- ARG[21] ce
  -> Signal dom (BitVector 16)   -- ARG[22] c
  -> Signal dom (BitVector 16)   -- ARG[23] a
  -> Signal dom (BitVector 16)   -- ARG[24] b
  -> Signal dom (BitVector 16)   -- ARG[25] d
  -> Signal dom Bit              -- ARG[26] irsttop
  -> Signal dom Bit              -- ARG[27] irstbot
  -> Signal dom Bit              -- ARG[28] orsttop
  -> Signal dom Bit              -- ARG[29] orstbot
  -> Signal dom Bit              -- ARG[30] ahold
  -> Signal dom Bit              -- ARG[31] bhold
  -> Signal dom Bit              -- ARG[32] chold
  -> Signal dom Bit              -- ARG[33] dhold
  -> Signal dom Bit              -- ARG[34] oholdtop
  -> Signal dom Bit              -- ARG[35] oholdbot
  -> Signal dom Bit              -- ARG[36] addsubtop
  -> Signal dom Bit              -- ARG[37] addsubbot
  -> Signal dom Bit              -- ARG[38] oloadtop
  -> Signal dom Bit              -- ARG[39] oloadbot
  -> Signal dom Bit              -- ARG[40] accumci
  -> Signal dom Bit              -- ARG[41] signextin
  -> Signal dom Bit              -- ARG[42] ci
  -> ( Signal dom (BitVector 32) -- o[31:0]
     , Signal dom Bit            -- co
     , Signal dom Bit            -- accumco
     , Signal dom Bit            -- signextout
     )"
       , "template" :
  "//SB_MAC16 begin
  wire [31:0]  ~GENSYM[o][0];
  wire         ~GENSYM[co][1];
  wire         ~GENSYM[accumco][2];
  wire         ~GENSYM[signextout][3];

  SB_MAC16 #(
    .NEG_TRIGGER(~ARG[0]),
    .C_REG(~ARG[3]),
    .A_REG(~ARG[1]),
    .B_REG(~ARG[2]),
    .D_REG(~ARG[4]),
    .TOP_8x8_MULT_REG(~ARG[5]),
    .BOT_8x8_MULT_REG(~ARG[6]),
    .PIPELINE_16x16_MULT_REG1(~ARG[7]),
    .PIPELINE_16x16_MULT_REG2(~ARG[8]),
    .TOPOUTPUT_SELECT(~ARG[9]),
    .TOPADDSUB_LOWERINPUT(~ARG[10]),
    .TOPADDSUB_UPPERINPUT(~ARG[11]),
    .TOPADDSUB_CARRYSELECT(~ARG[12]),
    .BOTOUTPUT_SELECT(~ARG[13]),
    .BOTADDSUB_LOWERINPUT(~ARG[14]),
    .BOTADDSUB_UPPERINPUT(~ARG[15]),
    .BOTADDSUB_CARRYSELECT(~ARG[16]),
    .MODE_8x8(~ARG[17]),
    .A_SIGNED(~ARG[18]),
    .B_SIGNED(~ARG[19])
  ) ~GENSYM[sb_mac16_inst][4] (
    .CLK(~ARG[20]),
    .CE(~ARG[21]),
    .C(~ARG[22]),
    .A(~ARG[23]),
    .B(~ARG[24]),
    .D(~ARG[25]),
    .AHOLD(~ARG[30]),
    .BHOLD(~ARG[31]),
    .CHOLD(~ARG[32]),
    .DHOLD(~ARG[33]),
    .IRSTTOP(~ARG[26]),
    .IRSTBOT(~ARG[27]),
    .ORSTTOP(~ARG[28]),
    .ORSTBOT(~ARG[29]),
    .OLOADTOP(~ARG[38]),
    .OLOADBOT(~ARG[39]),
    .ADDSUBTOP(~ARG[36]),
    .ADDSUBBOT(~ARG[37]),
    .OHOLDTOP(~ARG[34]),
    .OHOLDBOT(~ARG[35]),
    .CI(~ARG[42]),
    .ACCUMCI(~ARG[40]),
    .SIGNEXTIN(~ARG[41]),
    .O(~SYM[0]),
    .CO(~SYM[1]),
    .ACCUMCO(~SYM[2]),
    .SIGNEXTOUT(~SYM[3])
  );

  assign ~RESULT = { ~SYM[0], ~SYM[1], ~SYM[2], ~SYM[3] };
  // SB_MAC16 end"
       }
     }
  ]
  |]) #-}

-- | Multiply-Accumulate primitive
{-# NOINLINE macPrim #-}
macPrim
  :: Bit                         -- ^ negTrigger
  -> Bit                         -- ^ aReg
  -> Bit                         -- ^ bReg
  -> Bit                         -- ^ cReg
  -> Bit                         -- ^ dReg
  -> Bit                         -- ^ top8x8MultReg
  -> Bit                         -- ^ bot8x8MultReg
  -> Bit                         -- ^ pipeline16x16MultReg1
  -> Bit                         -- ^ pipeline16x16MultReg2
  -> BitVector 2                 -- ^ topOutputSelect
  -> BitVector 2                 -- ^ topAddSubLowerInput
  -> Bit                         -- ^ topAddSubUpperInput
  -> BitVector 2                 -- ^ topAddSubCarrySelect
  -> BitVector 2                 -- ^ botOutputSelect
  -> BitVector 2                 -- ^ botAddSubLowerInput
  -> Bit                         -- ^ botAddSubUpperInput
  -> BitVector 2                 -- ^ botAddSubCarrySelect
  -> Bit                         -- ^ mode8x8
  -> Bit                         -- ^ aSigned
  -> Bit                         -- ^ bSigned
  -> Clock dom                   -- ^ clk
  -> Signal dom Bit              -- ^ ce
  -> Signal dom (BitVector 16)   -- ^ c
  -> Signal dom (BitVector 16)   -- ^ a
  -> Signal dom (BitVector 16)   -- ^ b
  -> Signal dom (BitVector 16)   -- ^ d
  -> Signal dom Bit              -- ^ irsttop
  -> Signal dom Bit              -- ^ irstbot
  -> Signal dom Bit              -- ^ orsttop
  -> Signal dom Bit              -- ^ orstbot
  -> Signal dom Bit              -- ^ ahold
  -> Signal dom Bit              -- ^ bhold
  -> Signal dom Bit              -- ^ chold
  -> Signal dom Bit              -- ^ dhold
  -> Signal dom Bit              -- ^ oholdtop
  -> Signal dom Bit              -- ^ oholdbot
  -> Signal dom Bit              -- ^ addsubtop
  -> Signal dom Bit              -- ^ addsubbot
  -> Signal dom Bit              -- ^ oloadtop
  -> Signal dom Bit              -- ^ oloadbot
  -> Signal dom Bit              -- ^ accumci
  -> Signal dom Bit              -- ^ signextin
  -> Signal dom Bit              -- ^ ci
  -> ( Signal dom (BitVector 32) -- o[31:0]
     , Signal dom Bit            -- co
     , Signal dom Bit            -- accumco
     , Signal dom Bit            -- signextout
     )                           -- ^ (o[31:0], co, accumco, signextout)
macPrim !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ = (0, 0, 0, 0)
