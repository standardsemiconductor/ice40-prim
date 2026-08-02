# Revision history for ice40-prim

## Unreleased
* Mark primitives `OPAQUE` rather than `NOINLINE` on GHC >= 9.4. Clash 1.10
  warns that a primitive marked `NOINLINE` "might make Clash ignore this
  primitive". Guarded by CPP, so GHC 9.0 keeps `NOINLINE`.
* Update dependency bounds: clash-prelude-1.10, GHC 9.10
  * `clash-prelude >= 1.2.5 && < 1.11`
  * `base >= 4.12 && < 4.21`
* Test GHC 9.8 and 9.10 in CI

## 0.3.1.4 -- 2024-01-15
* Update dependency bounds: clash-prelude-1.8

## 0.3.1.3 -- 2022-02-13
* Update dependency bounds: clash-prelude-1.6.1
  * clash-prelude >= 1.2.5 && < 1.7

## 0.3.1.2 -- 2021-12-30
* Update dependency bounds

## 0.3.1.1 -- 2021-07-1

* Fix bit index operator spacing for GHC 9.0

## 0.3.1.0 -- 2021-03-30
* Add Global Buffer IP, Ice40.GB
* Update documentation

## 0.3.0.1 -- 2021-03-28

* Minor documentation updates
* Update dependency bounds

## 0.3.0.0 -- 2021-03-12

* Add PLL IP, pad and core
* Generate ice40 High Frequency Oscillator primitive instance

## 0.2.0.0 -- 2021-03-3

* Add MAC wrapper as Ice40.Mac, move MAC primitive to Ice40.Mac.Prim
* Additional documentation

## 0.1.0.0 -- 2021-02-15

* First version. 
