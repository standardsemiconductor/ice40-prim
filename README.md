# ice40-prim
![Haskell CI](https://github.com/standardsemiconductor/ice40-prim/workflows/Haskell%20CI/badge.svg)
[![Hackage][hackage-badge]][hackage]
[![Hackage Dependencies][hackage-deps-badge]][hackage-deps]

Lattice iCE40 Primitive IP
## Supported IP Modules
* [Ice40.Spram](https://github.com/standardsemiconductor/ice40-prim/blob/main/src/Ice40/Spram.hs) - For more information see the [iCE40 SPRAM Usage Guide](https://github.com/standardsemiconductor/VELDT-info/blob/master/FPGA-TN-02022-1-2-iCE40-SPRAM-Usage-Guide.pdf)
  * sysMem Single Port RAM Memory (SPRAM)
  * Each block of SPRAM is 16k x 16 (256 kbits)
  * 16-bit data width with nibble mast control
  * Cascadable design for deeper/wider SPRAM
  * Three power modes, standby, sleep, and power off

* [Ice40.Mac](https://github.com/standardsemiconductor/ice40-prim/blob/main/src/Ice40/Mac.hs) - For more information see the [DSP Function Usage Guide PDF](https://github.com/standardsemiconductor/VELDT-info/blob/master/DSPFunctionUsageGuideforICE40Devices.pdf)
  * 16-bit x 16-bit Multiplier, or two independent 8-bit x 8-bit multipliers
  * Optional independent pipeline control on input Register, Output Register, and Intermediate Register for faster clock performance
  * 32-bit accumulator, or two independent 16-bit accumulators
  * 32-bit, or two independent 16-bit adder/subtractor functions, registered or asynchronous
  * Cascadable to create wider accumulator blocks

* [Ice40.Osc](https://github.com/standardsemiconductor/ice40-prim/blob/main/src/Ice40/Osc.hs) - For more information see the [iCE40 Oscillator Usage Guide](https://github.com/standardsemiconductor/VELDT-info/blob/master/FPGA-TN-02008-1-7-iCE40-Oscillator-Usage-Guide.pdf)
  * on-chip oscillator
  * Low-power low frequency oscillator of 10 kHz
  * High frequency oscillator configurable to 48 Mhz, 24 Mhz, 12 Mhz, or 6 Mhz
  * See also [Ice40.Clock](https://github.com/standardsemiconductor/ice40-prim/blob/main/src/Ice40/Clock.hs) for clock domains and reset

* [Ice40.Pll](https://github.com/standardsemiconductor/ice40-prim/tree/main/src/Ice40/Pll) - For more information see the [iCE40 sysCLOCK PLL Design and Usage Guide](https://github.com/standardsemiconductor/VELDT-info/blob/master/iCE40sysCLOCKPLLDesignandUsageGuide.pdf)
  * [Pad](https://github.com/standardsemiconductor/ice40-prim/blob/main/src/Ice40/Pll/Pad.hs) and [Core](https://github.com/standardsemiconductor/ice40-prim/blob/main/src/Ice40/Pll/Core.hs) variants
  * Phase Lock Loop (PLL)
  * Provides a variety of user-synthesizable clock frequencies along with custom phase delays
  * Generates a new output clock freuquency via clock multiplication and division
  * De-skews or phase-aligns an output clock to the input reference clock
  * Corrects output clock to have nearly a 50% duty cycle, which is important for Double Data Rate (DDR) applications

* [Ice40.Rgb](https://github.com/standardsemiconductor/ice40-prim/blob/main/src/Ice40/Rgb.hs) - For more information see the [iCE40 LED Driver Usage Guide](https://github.com/standardsemiconductor/VELDT-info/blob/master/ICE40LEDDriverUsageGuide.pdf)
  * RGB High Current Drive I/O Pins
  * Provides sinking current to an LED connecting to the positive supply
  * Three outputs designed to drive the RGB LEDs
  * RGB drive current is user programmable from 4mA to 24mA, in increments of 4mA

* [Ice40.Led](https://github.com/standardsemiconductor/ice40-prim/blob/main/src/Ice40/Led.hs) - For more information see the [iCE40 LED Driver Usage Guide PDF](https://github.com/standardsemiconductor/VELDT-info/blob/master/ICE40LEDDriverUsageGuide.pdf)
  * LED PWM IP
  * Provide easier usage of RGB high current drivers
  * Provides flexibility for user to dynamically change the modulation width of each of the RGB LED driver
  * User can dynamically change ON and OFF-time durations
  * Ability to turn LEDs on and off gradually with breath-on and breath-off time

* [Ice40.Spi](https://github.com/standardsemiconductor/ice40-prim/blob/main/src/Ice40/Spi.hs) - For more information see the [Advanced SPI and I2C Usage Guide PDF](https://github.com/standardsemiconductor/VELDT-info/blob/master/AdvancediCE40SPII2CHardenedIPUsageGuide.pdf)
  * User SPI IP
  * Configurable Boss and Worker modes
  * Full-Duplex data transfer
  * Mode fault error flag with CPU interrupt capability
  * Double-buffered data register
  * Serial clock with programmable polarity and phase
  * LSB First or MSB First data transfer

* [Ice40.IO](https://github.com/standardsemiconductor/ice40-prim/blob/main/src/Ice40/IO.hs)
  * IO primitive

* [Ice40.GB](https://github.com/standardsemiconductor/ice40-prim/blob/main/src/Ice40/GB.hs)
  * Global buffer primitive
  * Required for a user's internally generated FPGA signal that is heavily loaded and requires global buffering; for example, a user's logic-generated clock

* [Ice40.I2c](https://github.com/standardsemiconductor/ice40-prim/blob/main/src/Ice40/I2c.hs) - For more information see the [Advanced SPI and I2C Usage Guide PDF](https://github.com/standardsemiconductor/VELDT-info/blob/master/AdvancediCE40SPII2CHardenedIPUsageGuide.pdf)
  * User I2C IP
  * Boss and Worker operation
  * 7-bit and 10-bit addressing
  * Multi-master arbitration support
  * Clock stretching
  * Up to 400 kHz data transfer speed
  * General Call support
  * Optionally delaying input or output data, or both
  * Optional filter on SCL input

## Lattice Documentation

[iCE40 UltraPlus Family Data Sheet PDF](https://github.com/standardsemiconductor/VELDT-info/blob/master/FPGA-DS-02008-1-9-iCE40-UltraPlus-Family-Data-Sheet.pdf)

[iCE Technology Library PDF](https://github.com/standardsemiconductor/VELDT-info/blob/master/SBTICETechnologyLibrary201708.pdf)

[Advanced SPI and I2C Usage Guide PDF](https://github.com/standardsemiconductor/VELDT-info/blob/master/AdvancediCE40SPII2CHardenedIPUsageGuide.pdf)

[iCE40 LED Driver Usage Guide PDF](https://github.com/standardsemiconductor/VELDT-info/blob/master/ICE40LEDDriverUsageGuide.pdf)

[iCE40 SPRAM Usage Guide](https://github.com/standardsemiconductor/VELDT-info/blob/master/FPGA-TN-02022-1-2-iCE40-SPRAM-Usage-Guide.pdf)

[DSP Function Usage Guide PDF](https://github.com/standardsemiconductor/VELDT-info/blob/master/DSPFunctionUsageGuideforICE40Devices.pdf)

[iCE40 sysCLOCK PLL Design and Usage Guide](https://github.com/standardsemiconductor/VELDT-info/blob/master/iCE40sysCLOCKPLLDesignandUsageGuide.pdf)

[hackage]:            <https://hackage.haskell.org/package/ice40-prim>
[hackage-badge]:      <https://img.shields.io/hackage/v/ice40-prim.svg?color=success>
[hackage-deps-badge]: <https://img.shields.io/hackage-deps/v/ice40-prim.svg>
[hackage-deps]:       <http://packdeps.haskellers.com/feed?needle=ice40-prim>
