module Ice40.I2c where

import Clash.Prelude
import Clash.Annotations.Primitive
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)

{-# ANN i2cPrim (InlinePrimitive [Verilog] $ unindent [i|
  [  { "BlackBox" :
       { "name" : "Ice40.I2c.i2cPrim"
       , "kind" : "Declaration"
       , "type" :
  "i2cPrim
    :: String          -- ARG[0]  initAddr
    -> String          -- ARG[1]  busAddr
    -> Clock dom       -- ARG[2]  sbclki
    -> Signal dom Bool -- ARG[3]  sbrwi
    -> Signal dom Bool -- ARG[4]  sbstbi
    -> Signal dom Bit  -- ARG[5]  sbadri7
    -> Signal dom Bit  -- ARG[6]  sbadri6
    -> Signal dom Bit  -- ARG[7]  sbadri5
    -> Signal dom Bit  -- ARG[8]  sbadri4
    -> Signal dom Bit  -- ARG[9]  sbadri3
    -> Signal dom Bit  -- ARG[10] sbadri2
    -> Signal dom Bit  -- ARG[11] sbadri1
    -> Signal dom Bit  -- ARG[12] sbadri0
    -> Signal dom Bit  -- ARG[13] sbdati7
    -> Signal dom Bit  -- ARG[14] sbdati6
    -> Signal dom Bit  -- ARG[15] sbdati5
    -> Signal dom Bit  -- ARG[16] sbdati4
    -> Signal dom Bit  -- ARG[17] sbdati3
    -> Signal dom Bit  -- ARG[18] sbdati2
    -> Signal dom Bit  -- ARG[19] sbdati1
    -> Signal dom Bit  -- ARG[20] sbdati0
    -> Signal dom Bit  -- ARG[21] scli
    -> Signal dom Bit  -- ARG[22] sdai
    -> Unbundled dom
         ( BitVector 8 -- sbdato
         , Bool        -- sbacko
         , Bit         -- i2cirq
         , Bit         -- i2cwkup
         , Bit         -- sclo
         , Bit         -- scloe
         , Bit         -- sdao
         , Bit         -- sdaoe
         )"
       , "template" :
  "//SB_I2C begin
  wire ~GENSYM[sbdato7][0];
  wire ~GENSYM[sbdato6][1];
  wire ~GENSYM[sbdato5][2];
  wire ~GENSYM[sbdato4][3];
  wire ~GENSYM[sbdato3][4];
  wire ~GENSYM[sbdato2][5];
  wire ~GENSYM[sbdato1][6];
  wire ~GENSYM[sbdato0][7];
  wire ~GENSYM[sbacko][8];
  wire ~GENSYM[i2cirq][9];
  wire ~GENSYM[i2cwkup][10];
  wire ~GENSYM[sclo][11];
  wire ~GENSYM[scloe][12];
  wire ~GENSYM[sdao][13];
  wire ~GENSYM[sdaoe][14];

  SB_I2C #(
    .I2C_SLAVE_INIT_ADDR(~ARG[0]),
    .BUS_ADDR74(~ARG[1])
  ) ~GENSYM[sb_i2c_inst][15] (
    .SBCLKI(~ARG[2]),
    .SBRWI(~ARG[3]),
    .SBSTBI(~ARG[4]),
    .SBADRI7(~ARG[5]),
    .SBADRI6(~ARG[6]),
    .SBADRI5(~ARG[7]),
    .SBADRI4(~ARG[8]),
    .SBADRI3(~ARG[9]),
    .SBADRI2(~ARG[10]),
    .SBADRI1(~ARG[11]),
    .SBADRI0(~ARG[12]),
    .SBDATI7(~ARG[13]),
    .SBDATI6(~ARG[14]),
    .SBDATI5(~ARG[15]),
    .SBDATI4(~ARG[16]),
    .SBDATI3(~ARG[17]),
    .SBDATI2(~ARG[18]),
    .SBDATI1(~ARG[19]),
    .SBDATI0(~ARG[20]),
    .SCLI(~ARG[21]),
    .SDAI(~ARG[22]),
    .SBDATO7(~SYM[0]),
    .SBDATO6(~SYM[1]),
    .SBDATO5(~SYM[2]),
    .SBDATO4(~SYM[3]),
    .SBDATO3(~SYM[4]),
    .SBDATO2(~SYM[5]),
    .SBDATO1(~SYM[6]),
    .SBDATO0(~SYM[7]),
    .SBACKO(~SYM[8]),
    .I2CIRQ(~SYM[9]),
    .I2CWKUP(~SYM[10]),
    .SCLO(~SYM[11]),
    .SCLOE(~SYM[12]),
    .SDAO(~SYM[13]),
    .SDAOE(~SYM[14])
  );

  assign ~RESULT = { ~SYM[0]  -- sbdato7
                   , ~SYM[1]  -- sbdato6
                   , ~SYM[2]  -- sbdato5
                   , ~SYM[3]  -- sbdato4
                   , ~SYM[4]  -- sbdato3
                   , ~SYM[5]  -- sbdato2
                   , ~SYM[6]  -- sbdato1
                   , ~SYM[7]  -- sbdato0
                   , ~SYM[8]  -- sbacko
                   , ~SYM[9]  -- i2cirq
                   , ~SYM[10] -- i2cwkup
                   , ~SYM[11] -- sclo
                   , ~SYM[12] -- scloe
                   , ~SYM[13] -- sdao
                   , ~SYM[14] -- sdaoe
                   };
  //SB_I2C end"
       }
     }
  ]
  |]) #-}

{-# NOINLINE i2cPrim #-}
i2cPrim
  :: String          -- ARG[0]  initAddr
  -> String          -- ARG[1]  busAddr
  -> Clock dom       -- ARG[2]  sbclki
  -> Signal dom Bool -- ARG[3]  sbrwi
  -> Signal dom Bool -- ARG[4]  sbstbi
  -> Signal dom Bit  -- ARG[5]  sbadri7
  -> Signal dom Bit  -- ARG[6]  sbadri6
  -> Signal dom Bit  -- ARG[7]  sbadri5
  -> Signal dom Bit  -- ARG[8]  sbadri4
  -> Signal dom Bit  -- ARG[9]  sbadri3
  -> Signal dom Bit  -- ARG[10] sbadri2
  -> Signal dom Bit  -- ARG[11] sbadri1
  -> Signal dom Bit  -- ARG[12] sbadri0
  -> Signal dom Bit  -- ARG[13] sbdati7
  -> Signal dom Bit  -- ARG[14] sbdati6
  -> Signal dom Bit  -- ARG[15] sbdati5
  -> Signal dom Bit  -- ARG[16] sbdati4
  -> Signal dom Bit  -- ARG[17] sbdati3
  -> Signal dom Bit  -- ARG[18] sbdati2
  -> Signal dom Bit  -- ARG[19] sbdati1
  -> Signal dom Bit  -- ARG[20] sbdati0
  -> Signal dom Bit  -- ARG[21] scli
  -> Signal dom Bit  -- ARG[22] sdai
  -> Unbundled dom
       ( BitVector 8 -- sbdato
       , Bool        -- sbacko
       , Bit         -- i2cirq
       , Bit         -- i2cwkup
       , Bit         -- sclo
       , Bit         -- scloe
       , Bit         -- sdao
       , Bit         -- sdaoe
       )
i2cPrim !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_
  = unbundle $ pure ( 0
                    , False
                    , 0
                    , 0
                    , 0
                    , 0
                    , 0
                    , 0
                    )
