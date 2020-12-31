module Ice40.Spi
  ( SpiIO
  , ToSb
  , FromSb
  , spiLeft
  , spiRight
  ) where

import Clash.Prelude
import Clash.Annotations.Primitive
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)
import Data.Maybe (isJust)
import Data.Functor ((<&>))

{-# ANN spiPrim (InlinePrimitive [Verilog] $ unindent [i|
  [  { "BlackBox" :
       { "name" : "Semi.Ice40.Spi.spiPrim"
       , "kind" : "Declaration"
       , "type" :
  "sbSpiPrim
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
    -> Unbundled dom 
         ( Bit         -- miso
         , Bit         -- mosi
         , Bit         -- sck
         , Bit         -- #cs
         , Bool        -- ack
         , BitVector 8 -- dat
         )"
        , "template" :
  "//SB_SPI begin
  wire ~GENSYM[mi][10];
  wire ~GENSYM[si][10];
  wire ~GENSYM[scki][17];
  wire ~GENSYM[scsni][24];
  wire ~GENSYM[sbdato7][1];
  wire ~GENSYM[sbdato6][2];
  wire ~GENSYM[sbdato5][3];
  wire ~GENSYM[sbdato4][4];
  wire ~GENSYM[sbdato3][5];
  wire ~GENSYM[sbdato2][6];
  wire ~GENSYM[sbdato1][7];
  wire ~GENSYM[sbdato0][8];
  wire ~GENSYM[sbacko][9];
  wire ~GENSYM[irq][??]  
  wire ~GENSYM[so][11];
  wire ~GENSYM[soe][12];
  wire ~GENSYM[mcsno3][13];
  wire ~GENSYM[mcsnoe3][14];
  wire ~GENSYM[mo][15];
  wire ~GENSYM[moe][16];
  wire ~GENSYM[scko][18];
  wire ~GENSYM[sckoe][19];
  wire ~GENSYM[miso][20];
  wire ~GENSYM[mosi][21];
  wire ~GENSYM[sck][22];
  wire ~GENSYM[cs][23];

  SB_SPI #(
    .BUS_ADDR74(~ARG[0])
  ) ~GENSYM[sb_spi_inst][0] (
    .SBCLKI(~ARG[1]),
    .SBRWI   (~ARG[2]),
    .SBSTBI  (~ARG[3]),
    .SBADRI7 (~ARG[4]),
    .SBADRI6 (~ARG[5]),
    .SBADRI5 (~ARG[6]),
    .SBADRI4 (~ARG[7]),
    .SBADRI3 (~ARG[8]),
    .SBADRI2 (~ARG[9]),
    .SBADRI1 (~ARG[10]),
    .SBADRI0 (~ARG[11]),
    .SBDATI7 (~ARG[12]),
    .SBDATI6 (~ARG[13]),
    .SBDATI5 (~ARG[14]),
    .SBDATI4 (~ARG[15]),
    .SBDATI3 (~ARG[16]),
    .SBDATI2 (~ARG[17]),
    .SBDATI1 (~ARG[18]),
    .SBDATI0 (~ARG[19]),

    .MI      (~SYM[1]),
    .SI      (~SYM[2]),
    .SCKI    (~SYM[3]),
    .SCSNI   (~SYM[4]), // 1'b1 ~SYM[24] NEEDS PULLUP
    .SBDATO7 (~SYM[5]),
    .SBDATO6 (~SYM[6]),
    .SBDATO5 (~SYM[7]),
    .SBDATO4 (~SYM[8]),
    .SBDATO3 (~SYM[9]),
    .SBDATO2 (~SYM[10]),
    .SBDATO1 (~SYM[11]),
    .SBDATO0 (~SYM[12]),
    .SBACKO  (~SYM[13]),
    .SPIIRQ  (~SYM[14]),
    .SPIWKUP (~SYM[15]),
    .SO      (~SYM[16]),
    .SOE     (~SYM[17]),
    .MO      (~SYM[18]),
    .MOE     (~SYM[19]),
    .SCKO    (~SYM[20]),
    .SCKOE   (~SYM[21]),
    .MCSNO3  (~SYM[22]),
    .MCSNO2  (~SYM[23]),
    .MCSNO1  (~SYM[24]),
    .MCSNO0  (~SYM[25]),
    .MCSNOE3 (~SYM[26]),
    .MCSNOE2 (~SYM[27]),
    .MCSNOE1 (~SYM[28]),
    .MCSNOE0 (~SYM[29])
  );

  SB_IO #(
    .PIN_TYPE(6'b101001),
    .PULLUP(1'b1)
  ) ~GENSYM[miso_io][26] (
    .PACKAGE_PIN(~SYM[20]),   // miso
    .OUTPUT_ENABLE(~SYM[12]), // soe
    .D_OUT_0(~SYM[11]),       // so
    .D_IN_0(~SYM[9])          // mi
  );

  SB_IO #(.PIN_TYPE(6'b101001)) ~GENSYM[mosi_io][27] (
    .PACKAGE_PIN(~SYM[21]),   // mosi
    .OUTPUT_ENABLE(~SYM[16]), // moe
    .D_OUT_0(~SYM[15]),       // mo
    .D_IN_0(~SYM[10])         // si
  );

  SB_IO #(
    .PIN_TYPE(6'b101001), 
    .PULLUP(1'b1)
  ) ~GENSYM[sck_io][28] (
    .PACKAGE_PIN(~SYM[22]),   // sck
    .OUTPUT_ENABLE(~SYM[19]), // sckoe
    .D_OUT_0(~SYM[18]),       // scko
    .D_IN_0(~SYM[17])         // scki
  );

  SB_IO #(
    .PIN_TYPE(6'b101001),
    .PULLUP(1'b1)
  ) ~GENSYM[cs_io][29] (
    .PACKAGE_PIN(~SYM[23]),   // cs
    .OUTPUT_ENABLE(~SYM[14]), // mscnoe3
    .D_OUT_0(~SYM[13]),       // mscno3
    .D_IN_0(~SYM[24])         // scsni unused
  );

  assign ~RESULT = { ~SYM[20] // miso
                   , ~SYM[21] // mosi
                   , ~SYM[22] // sck
                   , ~SYM[23] // #cs
                   , ~SYM[8]  // sbacko
                   , ~SYM[7]  // sbdato7
                   , ~SYM[6]  // sbdato6
                   , ~SYM[5]  // sbdato5
                   , ~SYM[4]  // sbdato4
                   , ~SYM[3]  // sbdato3
                   , ~SYM[2]  // sbdato2
                   , ~SYM[1]  // sbdato1
                   , ~SYM[0]  // sbdato0 
                   };
  // SB_SPI end"
       }
     }
  ]
  |]) #-}

{-# NOINLINE spiPrim #-}
spiPrim
  :: String            -- ARG[0]  busAddr
  -> Clock dom         -- ARG[1]  sbclki
  -> Signal dom Bool   -- ARG[2]  sbrwi
  -> Signal dom Bool   -- ARG[3]  sbstbi
  -> Signal dom Bit    -- ARG[4]  sbadri7
  -> Signal dom Bit    -- ARG[5]  sbadri6
  -> Signal dom Bit    -- ARG[6]  sbadri5
  -> Signal dom Bit    -- ARG[7]  sbadri4
  -> Signal dom Bit    -- ARG[8]  sbadri3
  -> Signal dom Bit    -- ARG[9] sbadri2
  -> Signal dom Bit    -- ARG[10] sbadri1
  -> Signal dom Bit    -- ARG[11] sbadri0
  -> Signal dom Bit    -- ARG[12] sbdati7
  -> Signal dom Bit    -- ARG[13] sbdati6
  -> Signal dom Bit    -- ARG[14] sbdati5
  -> Signal dom Bit    -- ARG[15] sbdati4
  -> Signal dom Bit    -- ARG[16] sbdati3
  -> Signal dom Bit    -- ARG[17] sbdati2
  -> Signal dom Bit    -- ARG[18] sbdati1
  -> Signal dom Bit    -- ARG[19] sbdati0
  -> Unbundled dom
       ( Bit           -- biwo
       , Bit           -- bowi
       , Bit           -- sck
       , Bit           -- #cs
       , Bool          -- ack
       , BitVector 8   -- dat
       )
spiPrim !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_
  = unbundle $ pure ( 0     -- biwo
                    , 0     -- bowi
                    , 0     -- sck
                    , 1     -- #cs
                    , False -- ack
                    , 0     -- dato
                    )

type Byte   = BitVector 8
type Addr   = Byte
type Data   = Byte
type ToSb   = Maybe (Addr, Maybe Data)
type FromSb = Maybe Data

data SpiIO = SpiIO
  { biwo :: "biwo" ::: Bit
  , bowi :: "bowi" ::: Bit
  , wck  :: "wck"  ::: Bit
  , cs   :: "cs"   ::: Bit
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

spi
  :: HiddenClockResetEnable dom
  => String                           -- busAddr
  -> Signal dom ToSb
  -> Unbundled dom (SpiIO, FromSb)
spi busAddr sbS
  = let (biwo', bowi', sck', cs', ack', dato') = spiPrim busAddr
                                                         hasClock
                                                         rw
                                                         stb
                                                         (bitAt 7 <$> sbadr)
                                                         (bitAt 6 <$> sbadr)
                                                         (bitAt 5 <$> sbadr)
                                                         (bitAt 4 <$> sbadr)
                                                         (bitAt 3 <$> sbadr)
                                                         (bitAt 2 <$> sbadr)
                                                         (bitAt 1 <$> sbadr)
                                                         (bitAt 0 <$> sbadr)
                                                         (bitAt 7 <$> sbdat)
                                                         (bitAt 6 <$> sbdat)
                                                         (bitAt 5 <$> sbdat)
                                                         (bitAt 4 <$> sbdat)
                                                         (bitAt 3 <$> sbdat)
                                                         (bitAt 2 <$> sbdat)
                                                         (bitAt 1 <$> sbdat)
                                                         (bitAt 0 <$> sbdat)
     in ( SpiIO <$> biwo'
                  <*> bowi'
                  <*> sck'
                  <*> cs'
        , fromSb <$> ack' <*> dato'
        )
  where
    fromSb ack' dato' = if ack'
      then Just dato'
      else Nothing
    rw = maybe False (isJust.snd) <$> sbS
    stb = isJust <$> sbS
    (sbadr, sbdat) = unbundle $ sbS <&> \case
      Nothing           -> (0, 0)
      Just (a, Nothing) -> (a, 0)
      Just (a, Just d)  -> (a, d)

bitAt :: Index 8 -> Byte -> Bit
bitAt n = (!n)

spiLeft
  :: HiddenClockResetEnable dom
  => Signal dom ToSb
  -> Unbundled dom (SpiIO, FromSb)
spiLeft = spi "0b0000"

spiRight
  :: HiddenClockResetEnable dom
  => Signal dom ToSb                
  -> Unbundled dom (SpiIO, FromSb)
spiRight = spi "0b0010"
