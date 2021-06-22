{-|
Module      : Ice40.Spi
Description : Ice40 SPI hard IP primitive
Copyright   : (c) David Cox, 2021
License     : BSD 3-Clause
Maintainer  : standardsemiconductor@gmail.com

SPI hard IP primitive from [Lattice Ice Technology Library](https://github.com/standardsemiconductor/VELDT-info/blob/master/SBTICETechnologyLibrary201708.pdf)
-}
module Ice40.Spi ( spi ) where

import Clash.Prelude
import Clash.Annotations.Primitive
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)
                    
-- | spi primitive wrapper
spi
  :: HiddenClock dom
  => String                   -- ^ busAddr
  -> Signal dom Bool          -- ^ sbrwi
  -> Signal dom Bool          -- ^ sbstbi
  -> Signal dom (BitVector 8) -- ^ sbadri
  -> Signal dom (BitVector 8) -- ^ sbdati
  -> Signal dom Bit           -- ^ bi
  -> Signal dom Bit           -- ^ wi
  -> Signal dom Bit           -- ^ wcki
  -> Signal dom Bit           -- ^ wcsni
  -> Unbundled dom 
       ( BitVector 8 -- sbdato
       , Bool        -- sbacko
       , Bit         -- spiirq
       , Bit         -- spiwkup
       , Bit         -- wo
       , Bit         -- woe
       , Bit         -- bo
       , Bit         -- boe
       , Bit         -- wcko
       , Bit         -- wckoe
       , BitVector 4 -- bcsno
       , BitVector 4 -- bcsnoe
       ) -- ^ (sbdato, sbacko, spiirq, spiwkup, wo, woe, bo, boe, wcko, wckoe, bcsno, bcsnoe)
spi busAddr sbrwi sbstbi sbadri sbdati bi wi wcki wcsni =
  spiPrim busAddr
          hasClock
          sbrwi
          sbstbi
          (bitAt 7 sbadri)
          (bitAt 6 sbadri)
          (bitAt 5 sbadri)
          (bitAt 4 sbadri)
          (bitAt 3 sbadri)
          (bitAt 2 sbadri)
          (bitAt 1 sbadri)
          (bitAt 0 sbadri)
          (bitAt 7 sbdati)
          (bitAt 6 sbdati)
          (bitAt 5 sbdati)
          (bitAt 4 sbdati)
          (bitAt 3 sbdati)
          (bitAt 2 sbdati)
          (bitAt 1 sbdati)
          (bitAt 0 sbdati)
          bi
          wi
          wcki
          wcsni

bitAt :: KnownNat n => Index n -> Signal dom (BitVector n) -> Signal dom Bit
bitAt n = fmap (! n)

{-# ANN spiPrim (InlinePrimitive [Verilog] $ unindent [i|
  [  { "BlackBox" :
       { "name" : "Ice40.Spi.spiPrim"
       , "kind" : "Declaration"
       , "type" :
  "spiPrim
    :: String          -- ARG[0]  busAddr
    -> Clock dom       -- ARG[1]  sbclki
    -> Signal dom Bool -- ARG[2]  sbrwi
    -> Signal dom Bool -- ARG[3]  sbstbi
    -> Signal dom Bit  -- ARG[4]  sbadri7
    -> Signal dom Bit  -- ARG[5]  sbadri6
    -> Signal dom Bit  -- ARG[6]  sbadri5
    -> Signal dom Bit  -- ARG[7]  sbadri4
    -> Signal dom Bit  -- ARG[8]  sbadri3
    -> Signal dom Bit  -- ARG[9]  sbadri2
    -> Signal dom Bit  -- ARG[10] sbadri1
    -> Signal dom Bit  -- ARG[11] sbadri0
    -> Signal dom Bit  -- ARG[12] sbdati7
    -> Signal dom Bit  -- ARG[13] sbdati6
    -> Signal dom Bit  -- ARG[14] sbdati5
    -> Signal dom Bit  -- ARG[15] sbdati4
    -> Signal dom Bit  -- ARG[16] sbdati3
    -> Signal dom Bit  -- ARG[17] sbdati2
    -> Signal dom Bit  -- ARG[18] sbdati1
    -> Signal dom Bit  -- ARG[19] sbdati0
    -> Signal dom Bit  -- ARG[20] bi
    -> Signal dom Bit  -- ARG[21] wi
    -> Signal dom Bit  -- ARG[22] wcki
    -> Signal dom Bit  -- ARG[23] wcsni
    -> Unbundled dom 
         ( BitVector 8 -- sbdato
         , Bool        -- sbacko
         , Bit         -- spiirq
         , Bit         -- spiwkup
         , Bit         -- wo
         , Bit         -- woe
         , Bit         -- bo
         , Bit         -- boe
         , Bit         -- wcko
         , Bit         -- wckoe
         , BitVector 4 -- bcsno
         , BitVector 4 -- bcsnoe
         )"
        , "template" :
  "//SB_SPI begin
  wire ~GENSYM[sbdato7][0];
  wire ~GENSYM[sbdato6][1];
  wire ~GENSYM[sbdato5][2];
  wire ~GENSYM[sbdato4][3];
  wire ~GENSYM[sbdato3][4];
  wire ~GENSYM[sbdato2][5];
  wire ~GENSYM[sbdato1][6];
  wire ~GENSYM[sbdato0][7];
  wire ~GENSYM[sbacko][8];
  wire ~GENSYM[irq][9];
  wire ~GENSYM[wkup][10];
  wire ~GENSYM[wo][11];
  wire ~GENSYM[woe][12];
  wire ~GENSYM[bo][13];
  wire ~GENSYM[boe][14];
  wire ~GENSYM[wcko][15];
  wire ~GENSYM[wckoe][16];
  wire ~GENSYM[bcsno3][17];
  wire ~GENSYM[bcsno2][18];
  wire ~GENSYM[bcsno1][19];
  wire ~GENSYM[bcsno0][20];
  wire ~GENSYM[bcsnoe3][21];
  wire ~GENSYM[bcsnoe2][22];
  wire ~GENSYM[bcsnoe1][23];
  wire ~GENSYM[bcsnoe0][24];

  SB_SPI #(
    .BUS_ADDR74( ~ARG[0] )
  ) ~GENSYM[sb_spi_inst][25] (
    .SBCLKI  ( ~ARG[1]  ),
    .SBRWI   ( ~ARG[2]  ),
    .SBSTBI  ( ~ARG[3]  ),
    .SBADRI7 ( ~ARG[4]  ),
    .SBADRI6 ( ~ARG[5]  ),
    .SBADRI5 ( ~ARG[6]  ),
    .SBADRI4 ( ~ARG[7]  ),
    .SBADRI3 ( ~ARG[8]  ),
    .SBADRI2 ( ~ARG[9]  ),
    .SBADRI1 ( ~ARG[10] ),
    .SBADRI0 ( ~ARG[11] ),
    .SBDATI7 ( ~ARG[12] ),
    .SBDATI6 ( ~ARG[13] ),
    .SBDATI5 ( ~ARG[14] ),
    .SBDATI4 ( ~ARG[15] ),
    .SBDATI3 ( ~ARG[16] ),
    .SBDATI2 ( ~ARG[17] ),
    .SBDATI1 ( ~ARG[18] ),
    .SBDATI0 ( ~ARG[19] ),
    .MI      ( ~ARG[20] ),
    .SI      ( ~ARG[21] ),
    .SCKI    ( ~ARG[22] ),
    .SCSNI   ( ~ARG[23] ),

    .SBDATO7 ( ~SYM[0]  ),
    .SBDATO6 ( ~SYM[1]  ),
    .SBDATO5 ( ~SYM[2]  ),
    .SBDATO4 ( ~SYM[3]  ),
    .SBDATO3 ( ~SYM[4]  ),
    .SBDATO2 ( ~SYM[5]  ),
    .SBDATO1 ( ~SYM[6] ),
    .SBDATO0 ( ~SYM[7] ),
    .SBACKO  ( ~SYM[8] ),
    .SPIIRQ  ( ~SYM[9] ),
    .SPIWKUP ( ~SYM[10] ),
    .SO      ( ~SYM[11] ),
    .SOE     ( ~SYM[12] ),
    .MO      ( ~SYM[13] ),
    .MOE     ( ~SYM[14] ),
    .SCKO    ( ~SYM[15] ),
    .SCKOE   ( ~SYM[16] ),
    .MCSNO3  ( ~SYM[17] ),
    .MCSNO2  ( ~SYM[18] ),
    .MCSNO1  ( ~SYM[19] ),
    .MCSNO0  ( ~SYM[20] ),
    .MCSNOE3 ( ~SYM[21] ),
    .MCSNOE2 ( ~SYM[22] ),
    .MCSNOE1 ( ~SYM[23] ),
    .MCSNOE0 ( ~SYM[24] )
  );

  assign ~RESULT = { ~SYM[0]  // sbdato7
                   , ~SYM[1]  // sbdato6
                   , ~SYM[2]  // sbdato5
                   , ~SYM[3]  // sbdato4
                   , ~SYM[4]  // sbdato3
                   , ~SYM[5]  // sbdato2
                   , ~SYM[6]  // sbdato1
                   , ~SYM[7]  // sbdato0
                   , ~SYM[8]  // sbacko
                   , ~SYM[9]  // spiirq
                   , ~SYM[10] // spiwkup
                   , ~SYM[11] // wo
                   , ~SYM[12] // woe
                   , ~SYM[13] // bo
                   , ~SYM[14] // boe
                   , ~SYM[15] // wcko
                   , ~SYM[16] // wckoe
                   , ~SYM[17] // bcsno3
                   , ~SYM[18] // bcsno2
                   , ~SYM[19] // bcsno1
                   , ~SYM[20] // bcsno0
                   , ~SYM[21] // bcsnoe3
                   , ~SYM[22] // bcsnoe2
                   , ~SYM[23] // bcsnoe1
                   , ~SYM[24] // bcsnoe0
                   };
  // SB_SPI end"
       }
     }
  ]
  |]) #-}

{-# NOINLINE spiPrim #-}
spiPrim
  :: String          -- ARG[0]  busAddr
  -> Clock dom       -- ARG[1]  sbclki
  -> Signal dom Bool -- ARG[2]  sbrwi
  -> Signal dom Bool -- ARG[3]  sbstbi
  -> Signal dom Bit  -- ARG[4]  sbadri7
  -> Signal dom Bit  -- ARG[5]  sbadri6
  -> Signal dom Bit  -- ARG[6]  sbadri5
  -> Signal dom Bit  -- ARG[7]  sbadri4
  -> Signal dom Bit  -- ARG[8]  sbadri3
  -> Signal dom Bit  -- ARG[9]  sbadri2
  -> Signal dom Bit  -- ARG[10] sbadri1
  -> Signal dom Bit  -- ARG[11] sbadri0
  -> Signal dom Bit  -- ARG[12] sbdati7
  -> Signal dom Bit  -- ARG[13] sbdati6
  -> Signal dom Bit  -- ARG[14] sbdati5
  -> Signal dom Bit  -- ARG[15] sbdati4
  -> Signal dom Bit  -- ARG[16] sbdati3
  -> Signal dom Bit  -- ARG[17] sbdati2
  -> Signal dom Bit  -- ARG[18] sbdati1
  -> Signal dom Bit  -- ARG[19] sbdati0
  -> Signal dom Bit  -- ARG[20] bi
  -> Signal dom Bit  -- ARG[21] wi
  -> Signal dom Bit  -- ARG[22] wcki
  -> Signal dom Bit  -- ARG[23] wcsni
  -> Unbundled dom 
       ( BitVector 8 -- sbdato
       , Bool        -- sbacko
       , Bit         -- spiirq
       , Bit         -- spiwkup
       , Bit         -- wo
       , Bit         -- woe
       , Bit         -- bo
       , Bit         -- boe
       , Bit         -- wcko
       , Bit         -- wckoe
       , BitVector 4 -- bcsno
       , BitVector 4 -- bcsnoe
       )
spiPrim !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_
  = unbundle $ pure ( 0     -- sbdato
                    , False -- sbacko
                    , 0     -- spiirq
                    , 0     -- spiwkup
                    , 0     -- wo
                    , 0     -- woe
                    , 0     -- bo
                    , 0     -- boe
                    , 0     -- wcko
                    , 0     -- wckoe
                    , 0     -- bcsno
                    , 0     -- bcsnoe
                    )


