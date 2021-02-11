# ice40-prim

Lattice iCE40 Primitive IP
## Currently Supported IP
* [sysMem Single Port RAM Memory (SPRAM)]() - For more information see the [iCE40 SPRAM Usage Guide]()
  * Each block of SPRAM is 16k x 16 (256 kbits)
  * 16-bit data width with nibble mast control
  * Cascadable design for deeper/wider SPRAM
  * Three power modes, standby, sleep, and power off
* [sysDSP]() - For more information see the [DSP Function Usage Guide PDF]()
  * 16-bit x 16-bit Multiplier, or two independent 8-bit x 8-bit multipliers
  * Optional independent pipeline control on input Register, Output Register, and Intermediate Register for faster clock performance
  * 32-bit accumulator, or two independent 16-bit accumulators
  * 32-bit, or two independent 16-bit adder/subtractor functions, registered or asynchronous
  * Cascadable to create wider accumulator blocks
* [On-Chip Oscillator]()
  * Low-power low frequency oscillator of 10 kHz
  * High frequency oscillator configurable to 48 Mhz, 24 Mhz, 12 Mhz, or 6 Mhz
* [LED PWM IP]() - For more information see the [iCE40 LED Driver Usage Guide PDF]()
  * Provide easier usage of RGB high current drivers
  * Provides flexibility for user to dynamically change the modulation width of each of the RGB LED driver
  * User can dynamically change ON and OFF-time durations
  * Ability to turn LEDs on and off gradually with breath-on and breath-off time
* [User SPI IP]() - For more information see the [Advanced SPI and I2C Usage Guide PDF]()
  * Configurable Boss and Worker modes
  * Full-Duplex data transfer
  * Mode fault error flag with CPU interrupt capability
  * Double-buffered data register
  * Serial clock with programmable polarity and phase
  * LSB First or MSB First data transfer
* [User I2C IP]() - For more information see the [Advanced SPI and I2C Usage Guide PDF]()
  * Boss and Worker operation
  * 7-bit and 10-bit addressing
  * Multi-master arbitration support
  * Clock stretching
  * Up to 400 kHz data transfer speed
  * General Call support
  * Optionally delaying input or output data, or both
  * Optional filter on SCL input

## Lattice Documentation
[iCE40 UltraPlus Family Data Sheet PDF]()
[iCE Technology Library PDF]()
[Advanced SPI and I2C Usage Guide PDF]()
[iCE40 LED Driver Usage Guide PDF]()
