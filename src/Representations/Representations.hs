{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Representations.Representations
  (
    Closed(..)
  , Cartesian(..)
  , Constant(..)
  , Heyting(..)
  , HOL(..)
  ) where

import GHC.TypeLits
import Representations.Eval.Model


--------------------------------------------------------------------------------
-- | Algebras

class Closed repr where
  app :: repr (a -> b) -> repr a -> repr b
  lam :: (repr a -> repr b) -> repr (a -> b)

class Cartesian repr where
  unit :: repr ()
  pair :: repr a -> repr b -> repr (a, b)
  fst_ :: repr (a, b) -> repr a
  snd_ :: repr (a, b) -> repr b

class Constant a (str :: Symbol) repr where
  c :: repr a

class Heyting repr where
  (/\), (\/), (-->) :: repr Bool -> repr Bool -> repr Bool
  true, false :: repr Bool

class HOL a repr where
  forall, exists :: repr (a -> Bool) -> repr Bool
