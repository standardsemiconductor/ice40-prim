{-|
Module      : Ice40.Clock
Description : Ice40 Clock domains
Copyright   : (c) David Cox, 2021-2022
License     : BSD 3-Clause
Maintainer  : standardsemiconductor@gmail.com

Commonly used clock domains with ice40 IP
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ice40.Clock
  ( vLattice10kHz
  , Lattice10kHz
  , vLattice48Mhz
  , vLattice24Mhz
  , vLattice12Mhz
  , vLattice6Mhz
  , Lattice48Mhz
  , Lattice24Mhz
  , Lattice12Mhz
  , Lattice6Mhz
  , latticeRst
  ) where

import Clash.Prelude

createDomain vXilinxSystem{vName="Lattice10kHz", vPeriod=100000000}
createDomain vXilinxSystem{vName="Lattice48Mhz", vPeriod=20833}
createDomain vXilinxSystem{vName="Lattice24Mhz", vPeriod=41666}
createDomain vXilinxSystem{vName="Lattice12Mhz", vPeriod=83333}
createDomain vXilinxSystem{vName="Lattice6Mhz",  vPeriod=166660}

-- | Lattice reset signal, always inactive
latticeRst :: KnownDomain dom => Reset dom
latticeRst = unsafeFromActiveHigh $ pure False
