{-|
Module : Ice40.Pll.Pad
Description : Ice40 PLL Pad hard IP primitive
Copyright : (c) David Cox, 2021
License : BSD 3-Clause
Maintainer : standardsemiconductor@gmail.com

PLL Pad hard IP primitive from [Lattice Ice Technology Library](https://github.com/standardsemiconductor/VELDT-info/blob/master/SBTICETechnologyLibrary201708.pdf)
-}

module Ice40.Pll.Pad where

import Clash.Prelude
import Clash.Annotations.Primitive
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)