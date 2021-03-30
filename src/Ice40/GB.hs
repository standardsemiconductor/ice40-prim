{-|
Module      : Ice40.GB
Description : Ice40 Global Buffer IP primtive
Copyright   : (c) David Cox, 2021
License     : BSD-3-Clause
Maintainer  : standardsemiconductor@gmail.com

Global buffer IP primitive from [Lattice Ice Technology Library](https://github.com/standardsemiconductor/VELDT-info/blob/master/SBTICETechnologyLibrary201708.pdf).
Required for a user's internally generated FPGA signal that is heavily loaded and requires global buffering; for example, a user's logic-generated clock.
-}
module Ice40.GB where

import Clash.Prelude
import Clash.Annotations.Primitive
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)

{-# ANN gbPrim (InlinePrimitive [Verilog] $ unindent [i|
  [  { "BlackBox" :
       { "name" : "Ice40.GB.gbPrim"
       , "kind" : "Declaration"
       , "type" :
  "gbPrim
    :: Clock dom -- ARG[0]
    -> Clock dom"
       , "template" :
  "//SB_GB begin
  SB_GB ~GENSYM[sb_gb_inst][0] (
    .USER_SIGNAL_TO_GLOBAL_BUFFER ( ~ARG[0] ),
    .GLOBAL_BUFFER_OUTPUT         ( ~RESULT )
  );
  //SB_GB end"
       }
     }
  ]
  |]) #-}

-- | Global buffer primitive
{-# NOINLINE gbPrim #-}
gbPrim :: Clock dom -> Clock dom
gbPrim !clk = clk