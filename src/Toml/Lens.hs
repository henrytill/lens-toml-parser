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
  ( -- * Unannotated
    _Table,
    _List,
    _Double,
    _Integer,
    _Text,
    _Bool,
    _ZonedTime,
    _LocalTime,
    _Day,
    _TimeOfDay,

    -- * Annotated
    _Table',
    _List',
    _Double',
    _Integer',
    _Text',
    _Bool',
    _ZonedTime',
    _LocalTime',
    _Day',
    _TimeOfDay',
  )
where

import Data.Profunctor
import Data.Text (Text)
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

-- | @_List :: Prism' Value [Value]@
_List ::
  (Choice p, Applicative f) =>
  p [Value] (f [Value]) ->
  p Value (f Value)
_List =
  prism List $ \n -> case n of
    List v -> Right v
    _ -> Left n
{-# INLINE _List #-}

-- | @_Double :: Prism' Value Double@
_Double ::
  (Choice p, Applicative f) =>
  p Double (f Double) ->
  p Value (f Value)
_Double =
  prism Double $ \n -> case n of
    Double v -> Right v
    _ -> Left n
{-# INLINE _Double #-}

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

-- | @_Text :: Prism' Value Text@
_Text ::
  (Choice p, Applicative f) =>
  p Text (f Text) ->
  p Value (f Value)
_Text =
  prism Text $ \n -> case n of
    Text v -> Right v
    _ -> Left n
{-# INLINE _Text #-}

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

-- | @_Table' :: Prism' (Value' a) (a, Table' a)@
_Table' ::
  (Choice p, Applicative f) =>
  p (a, Table' a) (f (a, Table' a)) ->
  p (Value' a) (f (Value' a))
_Table' =
  prism (uncurry Table') $ \n -> case n of
    Table' a v -> Right (a, v)
    _ -> Left n
{-# INLINE _Table' #-}

-- | @_Array' :: Prism' (Value' a) (a, [Value' a])@
_List' ::
  (Choice p, Applicative f) =>
  p (a, [Value' a]) (f (a, [Value' a])) ->
  p (Value' a) (f (Value' a))
_List' =
  prism (uncurry List') $ \n -> case n of
    List' a v -> Right (a, v)
    _ -> Left n
{-# INLINE _List' #-}

-- | @_Double' :: Prism' (Value' a) (a, Double)@
_Double' ::
  (Choice p, Applicative f) =>
  p (a, Double) (f (a, Double)) ->
  p (Value' a) (f (Value' a))
_Double' =
  prism (uncurry Double') $ \n -> case n of
    Double' a v -> Right (a, v)
    _ -> Left n
{-# INLINE _Double' #-}

-- | @_Integer' :: Prism' (Value' a) (a, Integer)@
_Integer' ::
  (Choice p, Applicative f) =>
  p (a, Integer) (f (a, Integer)) ->
  p (Value' a) (f (Value' a))
_Integer' =
  prism (uncurry Integer') $ \n -> case n of
    Integer' a v -> Right (a, v)
    _ -> Left n
{-# INLINE _Integer' #-}

-- | @_Text' :: Prism' (Value' a) (a, Text)@
_Text' ::
  (Choice p, Applicative f) =>
  p (a, Text) (f (a, Text)) ->
  p (Value' a) (f (Value' a))
_Text' =
  prism (uncurry Text') $ \n -> case n of
    Text' a v -> Right (a, v)
    _ -> Left n
{-# INLINE _Text' #-}

-- | @_Bool' :: Prism' (Value' a) (a, Bool)@
_Bool' ::
  (Choice p, Applicative f) =>
  p (a, Bool) (f (a, Bool)) ->
  p (Value' a) (f (Value' a))
_Bool' =
  prism (uncurry Bool') $ \n -> case n of
    Bool' a v -> Right (a, v)
    _ -> Left n
{-# INLINE _Bool' #-}

-- | @_ZonedTime' :: Prism' (Value' a) (a, Time.ZonedTime)@
_ZonedTime' ::
  (Choice p, Applicative f) =>
  p (a, Time.ZonedTime) (f (a, Time.ZonedTime)) ->
  p (Value' a) (f (Value' a))
_ZonedTime' =
  prism (uncurry ZonedTime') $ \n -> case n of
    ZonedTime' a v -> Right (a, v)
    _ -> Left n
{-# INLINE _ZonedTime' #-}

-- | @_LocalTime' :: Prism' (Value' a) (a, Time.LocalTime)@
_LocalTime' ::
  (Choice p, Applicative f) =>
  p (a, Time.LocalTime) (f (a, Time.LocalTime)) ->
  p (Value' a) (f (Value' a))
_LocalTime' =
  prism (uncurry LocalTime') $ \n -> case n of
    LocalTime' a v -> Right (a, v)
    _ -> Left n
{-# INLINE _LocalTime' #-}

-- | @_Day' :: Prism' (Value' a) (a, Time.Day)@
_Day' ::
  (Choice p, Applicative f) =>
  p (a, Time.Day) (f (a, Time.Day)) ->
  p (Value' a) (f (Value' a))
_Day' =
  prism (uncurry Day') $ \n -> case n of
    Day' a v -> Right (a, v)
    _ -> Left n
{-# INLINE _Day' #-}

-- | @_TimeOfDay' :: Prism' (Value' a) (a, Time.TimeOfDay)@
_TimeOfDay' ::
  (Choice p, Applicative f) =>
  p (a, Time.TimeOfDay) (f (a, Time.TimeOfDay)) ->
  p (Value' a) (f (Value' a))
_TimeOfDay' =
  prism (uncurry TimeOfDay') $ \n -> case n of
    TimeOfDay' a v -> Right (a, v)
    _ -> Left n
{-# INLINE _TimeOfDay' #-}
