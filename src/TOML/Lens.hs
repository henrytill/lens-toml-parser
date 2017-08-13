-- |
-- Module      : TOML.Lens
-- Description : Lenses for toml-parser
-- Copyright   : (c) 2017, Henry Till
-- License     : ISC
-- Maintainer  : henrytill@gmail.com
-- Stability   : experimental
--
-- With help from:
-- <https://github.com/ekmett/lens/wiki/How-can-I-write-lenses-without-depending-on-lens%3F>
--
module TOML.Lens
  ( _Table
  , _List
  , _Double
  , _Integer
  , _String
  , _Bool
  , _ZonedTimeV
  , _LocalTimeV
  , _DayV
  , _TimeOfDayV
  ) where

import           Data.Profunctor
import qualified Data.Text       as T
import           Data.Time

import           TOML

--
-- @
-- type Prism s t a b = (Choice p, Applicative f) => p a (f b) -> p s (f t)
-- @
--
-- @
-- type Prism' s a = Prism s s a a
-- @
--

prism
  :: (Choice p, Applicative f)
  => (b -> t)
  -> (s -> Either t a)
  -> p a (f b)
  -> p s (f t)
prism bt seta = dimap seta (either pure (fmap bt)) . right'
{-# INLINE prism #-}

-- | @_Table :: Prism' Value [(Text, Value)]@
_Table
  :: (Choice p, Applicative f)
  => p [(T.Text, Value)] (f [(T.Text, Value)])
  -> p Value (f Value)
_Table =
  prism Table $ \ n -> case n of
    Table v -> pure v
    _       -> Left n

-- | @_List :: Prism' Value [Value]@
_List
  :: (Choice p, Applicative f)
  => p [Value] (f [Value])
  -> p Value (f Value)
_List =
  prism List $ \ n -> case n of
    List v -> pure v
    _      -> Left n

-- | @_Double :: Prism' Value Double@
_Double
  :: (Choice p, Applicative f)
  => p Double (f Double)
  -> p Value (f Value)
_Double =
  prism Double $ \ n -> case n of
    Double v -> pure v
    _        -> Left n

-- | @_Integer :: Prism' Value Integer@
_Integer
  :: (Choice p, Applicative f)
  => p Integer (f Integer)
  -> p Value (f Value)
_Integer =
  prism Integer $ \ n -> case n of
    Integer v -> pure v
    _         -> Left n

-- | @_String :: Prism' Value T.Text@
_String
  :: (Choice p, Applicative f)
  => p T.Text (f T.Text)
  -> p Value (f Value)
_String =
  prism String $ \ n -> case n of
    String v -> pure v
    _        -> Left n

-- | @_Bool :: Prism' Value Bool@
_Bool
  :: (Choice p, Applicative f)
  => p Bool (f Bool)
  -> p Value (f Value)
_Bool =
  prism Bool $ \ n -> case n of
    Bool v -> pure v
    _      -> Left n

-- | @_ZonedTimeV :: Prism' Value ZonedTime@
_ZonedTimeV
  :: (Choice p, Applicative f)
  => p ZonedTime (f ZonedTime)
  -> p Value (f Value)
_ZonedTimeV =
  prism ZonedTimeV $ \ n -> case n of
    ZonedTimeV v -> pure v
    _            -> Left n

-- | @_LocalTimeV :: Prism' Value LocalTime@
_LocalTimeV
  :: (Choice p, Applicative f)
  => p LocalTime (f LocalTime)
  -> p Value (f Value)
_LocalTimeV =
  prism LocalTimeV $ \ n -> case n of
    LocalTimeV v -> pure v
    _            -> Left n

-- | @_DayV :: Prism' Value Day@
_DayV
  :: (Choice p, Applicative f)
  => p Day (f Day)
  -> p Value (f Value)
_DayV =
  prism DayV $ \ n -> case n of
    DayV v -> pure v
    _      -> Left n

-- | @_TimeOfDayV :: Prism' Value TimeOfDay@
_TimeOfDayV
  :: (Choice p, Applicative f)
  => p TimeOfDay (f TimeOfDay)
  -> p Value (f Value)
_TimeOfDayV =
  prism TimeOfDayV $ \ n -> case n of
    TimeOfDayV v -> pure v
    _            -> Left n
