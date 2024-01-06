-- |
-- Module      : Toml.Lens
-- Description : Lenses for toml-parser
-- Copyright   : (c) 2017-2024, Henry Till
-- License     : ISC
-- Maintainer  : henrytill@gmail.com
-- Stability   : experimental
--
-- Lenses for <https://hackage.haskell.org/package/toml-parser toml-parser>.
module Toml.Lens
  ( _Table,
    _Array,
    _Float,
    _Integer,
    _String,
    _Bool,
    _ZonedTime,
    _LocalTime,
    _Day,
    _TimeOfDay,
  )
where

import Data.Profunctor
import qualified Data.Time as Time
import Toml

--
-- With help from:
-- <https://github.com/ekmett/lens/wiki/How-can-I-write-lenses-without-depending-on-lens%3F>
--
-- @
-- type Prism s t a b = (Choice p, Applicative f) => p a (f b) -> p s (f t)
-- @
--
-- @
-- type Prism' s a = Prism s s a a
-- @
--

prism ::
  (Choice p, Applicative f) =>
  (b -> t) ->
  (s -> Either t a) ->
  p a (f b) ->
  p s (f t)
prism bt seta = dimap seta (either pure (fmap bt)) . right'
{-# INLINE prism #-}

-- | @_Table :: Prism' Value Table@
_Table ::
  (Choice p, Applicative f) =>
  p Table (f Table) ->
  p Value (f Value)
_Table =
  prism Table $ \n -> case n of
    Table v -> Right v
    _ -> Left n
{-# INLINE _Table #-}

-- | @_Array :: Prism' Value [Value]@
_Array ::
  (Choice p, Applicative f) =>
  p [Value] (f [Value]) ->
  p Value (f Value)
_Array =
  prism Array $ \n -> case n of
    Array v -> Right v
    _ -> Left n
{-# INLINE _Array #-}

-- | @_Float :: Prism' Value Double@
_Float ::
  (Choice p, Applicative f) =>
  p Double (f Double) ->
  p Value (f Value)
_Float =
  prism Float $ \n -> case n of
    Float v -> Right v
    _ -> Left n
{-# INLINE _Float #-}

-- | @_Integer :: Prism' Value Integer@
_Integer ::
  (Choice p, Applicative f) =>
  p Integer (f Integer) ->
  p Value (f Value)
_Integer =
  prism Integer $ \n -> case n of
    Integer v -> Right v
    _ -> Left n
{-# INLINE _Integer #-}

-- | @_String :: Prism' Value String@
_String ::
  (Choice p, Applicative f) =>
  p String (f String) ->
  p Value (f Value)
_String =
  prism String $ \n -> case n of
    String v -> Right v
    _ -> Left n
{-# INLINE _String #-}

-- | @_Bool :: Prism' Value Bool@
_Bool ::
  (Choice p, Applicative f) =>
  p Bool (f Bool) ->
  p Value (f Value)
_Bool =
  prism Bool $ \n -> case n of
    Bool v -> Right v
    _ -> Left n
{-# INLINE _Bool #-}

-- | @_ZonedTime :: Prism' Value Time.ZonedTime@
_ZonedTime ::
  (Choice p, Applicative f) =>
  p Time.ZonedTime (f Time.ZonedTime) ->
  p Value (f Value)
_ZonedTime =
  prism ZonedTime $ \n -> case n of
    ZonedTime v -> Right v
    _ -> Left n
{-# INLINE _ZonedTime #-}

-- | @_LocalTime :: Prism' Value Time.LocalTime@
_LocalTime ::
  (Choice p, Applicative f) =>
  p Time.LocalTime (f Time.LocalTime) ->
  p Value (f Value)
_LocalTime =
  prism LocalTime $ \n -> case n of
    LocalTime v -> Right v
    _ -> Left n
{-# INLINE _LocalTime #-}

-- | @_Day :: Prism' Value Time.Day@
_Day ::
  (Choice p, Applicative f) =>
  p Time.Day (f Time.Day) ->
  p Value (f Value)
_Day =
  prism Day $ \n -> case n of
    Day v -> Right v
    _ -> Left n
{-# INLINE _Day #-}

-- | @_TimeOfDay :: Prism' Value Time.TimeOfDay@
_TimeOfDay ::
  (Choice p, Applicative f) =>
  p Time.TimeOfDay (f Time.TimeOfDay) ->
  p Value (f Value)
_TimeOfDay =
  prism TimeOfDay $ \n -> case n of
    TimeOfDay v -> Right v
    _ -> Left n
{-# INLINE _TimeOfDay #-}
