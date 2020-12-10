module Ice40.Mac where

import Clash.Prelude
import Clash.Annotations.Primitive
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)

{-# ANN macPrim (InlinePrimitive [Verilog] $ unindent [i|
  [  { "BlackBox" :
       { "name" : "Semi.Ice40.Mac.macPrim"
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

{-# NOINLINE macPrim #-}
macPrim
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
     )
macPrim !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ = (0, 0, 0, 0)

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

mac
  :: Clock dom
  -> Parameter
  -> Input dom
  -> ( Signal dom (BitVector 32)
     , Signal dom Bit
     , Signal dom Bit
     , Signal dom Bit
     )
mac clki Parameter{..} Input{..}
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
            clki
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

type Byte = BitVector 8

--------------------
-- 8x8 Multiplier --
--------------------
mult8x8BypassUnsigned
  :: HiddenClock dom
  => Signal dom Byte -- w
  -> Signal dom Byte -- x
  -> Signal dom Byte -- y
  -> Signal dom Byte -- z
  -> Signal dom (BitVector 16, BitVector 16) -- (w * x, y * z)
mult8x8BypassUnsigned w x y z = split <$> o
  where
    (o, _, _, _) = mac hasClock parameter input
    input = defaultInput{ a = (++#) <$> w <*> y
                        , b = (++#) <$> x <*> z
                        }
    parameter = defaultParameter{ mode8x8 = 1
                                , topOutputSelect = 2
                                , botOutputSelect = 2
                                }
                
----------------------
-- 16x16 Multiplier --
----------------------
mult16x16BypassUnsigned
  :: HiddenClock dom
  => Signal dom (BitVector 16)
  -> Signal dom (BitVector 16)
  -> Signal dom (BitVector 32)
mult16x16BypassUnsigned x y = o
  where
    (o, _, _, _) = mac hasClock parameter input
    input = defaultInput{ a = x
                        , b = y
                        }
    parameter = defaultParameter{ topOutputSelect = 3
                                , botOutputSelect = 3
                                }

-------------------
-- 16 Bit ADDSUB --
-------------------
-- | w +- x; TOP
--   y +- z; BOT
addSub16BypassedUnsigned
  :: HiddenClock dom
  => Signal dom Bit -- TOP; 0 = Add, 1 = Sub
  -> Signal dom Bit -- BOT; 0 = Add, 1 = Sub
  -> Signal dom (BitVector 16) -- w
  -> Signal dom (BitVector 16) -- x
  -> Signal dom (BitVector 16) -- y
  -> Signal dom (BitVector 16) -- z
  -> Unbundled dom (BitVector 16, BitVector 16)
addSub16BypassedUnsigned addSubTop addSubBot w x y z = unbundle $ split <$> o
  where
    (o, _, _, _) = mac hasClock parameter input
    input = defaultInput{ c = w
                        , a = x
                        , b = z
                        , d = y
                        , addsubtop = addSubTop
                        , addsubbot = addSubBot
                        }
    parameter = defaultParameter{ mode8x8 = 1
                                , botAddSubUpperInput = 1
                                , topAddSubUpperInput = 1
                                }
-------------------
-- 32 Bit ADDSUB --
-------------------
-- | Add: x + y
-- | Sub: x - y
addSub32BypassedUnsigned
  :: HiddenClock dom
  => Signal dom Bit -- 0 = Add, 1 = Sub
  -> Signal dom (BitVector 32) 
  -> Signal dom (BitVector 32)
  -> Unbundled dom (Bit, BitVector 32)
addSub32BypassedUnsigned addSub x y = (co, o)
  where
    (o, co, _, _) = mac hasClock parameter input
    input = defaultInput{ a = slice d31 d16 <$> y
                        , b = slice d15 d0  <$> y
                        , c = slice d31 d16 <$> x
                        , d = slice d15 d0  <$> x
                        , addsubtop = addSub
                        , addsubbot = addSub
                        }
    parameter = defaultParameter{ mode8x8 = 1
                                , botAddSubUpperInput = 1
                                , topAddSubCarrySelect = 2
                                , topAddSubUpperInput = 1
                                }

{-
addSub32PipelinedUnsigned
  :: HiddenClock dom
  => Signal dom Bit -- 0 = Add, 1 = Sub
  -> Signal dom Bit -- AHOLD
  -> Signal dom Bit -- BHOLD
  -> Signal dom Bit -- CHOLD
  -> Signal dom Bit -- DHOLD
  -> Signal dom (BitVector 32)
  -> Signal dom (BitVector 32)
  -> Unbundled dom (Bit, BitVector 32)
addSub32PipelinedUnsigned addSub aHold bHold cHold dHold x y = (co, o)
  where
    (o, co, _, _) = mac hasClock parameter input
    input = defaultInput{ a = slice d31 d16 <$> y
                        , b = slice d15 d0  <$> y
                        , c = slice d31 d16 <$> x
                        , d = slice d15 d0  <$> x
                        , addsubtop = addSub
                        , addsubbot = addSub
                        ,
-}
