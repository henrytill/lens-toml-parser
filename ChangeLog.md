# Revision history for lens-toml-parser

## 0.1.0.0  -- 2017-08-14

* First version. Released on an unsuspecting world.

## 0.1.0.1  -- 2017-10-07

* Added `INLINE` pragmas.
* Updated tests.

## 0.1.0.2  -- 2018-04-22

* Refactored (used `Right` instead of `pure` in prism definitions).
* Simplified tests/Main.hs to appease hlint.

## 0.1.0.3  -- 2018-11-14

* Loosened constraints on `base`, `containers`, `profunctors`, and `time`.
* Tweaked tests/Main.hs to appease hlint.

## 0.1.0.4  -- 2020-05-05

* Loosened constraints on `base`, `hlint`, `profunctors`, and `time`.

## 0.2.0.0  -- 2023-08-26

* Updated to toml-parser >=1.1.  This is a breaking change.

## 0.3.0.0  -- 2024-02-27

* Updated to toml-parser-2.0.0.0. This is a breaking change due to the
  new toml-parser using Text and different constructor names. It also
  adds new prisms supporting annotations used by the new toml-parser
  version for tracking file locations.

## 0.3.0.1  -- 2024-12-05

* Updated to dwergaz >=3.0.
* Tweaked tests.
