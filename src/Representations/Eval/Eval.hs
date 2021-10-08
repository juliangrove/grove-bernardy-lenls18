{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Representations.Eval.Eval
  (
    AsType
  , Ctx(..)
  , getProb
  , InCtx(..)
  , ProbI(..)
  , ProbProg(..)
  , runP
  , TradI(..)
  ) where

import Control.Monad
import GHC.TypeLits
import ProbProg.Gibbs
import ProbProg.Logits
import ProbProg.Histograms
import ProbProg.PreciseProb
import ProbProg.ProbLang
import Representations.Eval.Model
import Representations.Representations


--------------------------------------------------------------------------------
-- | Data

-- | The "traditional" interpretation
newtype TradI a = TradI { runTradI :: a } deriving (Eq, Show)

data AsType (k :: Symbol)

-- | Contexts
data Ctx repr k where
  Empty :: Ctx repr ()
  AddCon :: repr a -> Ctx repr p -> Ctx repr ((a, AsType str), p)

class InCtx a (str :: Symbol) k repr where
  π :: Ctx repr k -> repr a

instance {-# OVERLAPPING #-} InCtx a str ((a, AsType str), tail) repr where
  π (AddCon c tail) = c

instance InCtx t1 str k repr => InCtx t1 str (t2, k) repr where
  π (AddCon _ k) = π @t1 @str @k k

-- | The "probabilistic interpretation"
newtype ProbI k a = ProbI { runProbI :: Ctx TradI k -> TradI a }

-- | Probabilistic programs
data ProbProg a
  = PP { runPP :: (a -> P (Probabilistic Double)) -> P (Probabilistic Double) }
  deriving Functor

instance Applicative ProbProg where
  pure = PP . flip ($)
  (<*>) = ap

instance Monad ProbProg where
  (PP m) >>= k = PP $ \c -> m (\x -> runPP (k x) c)

runP :: Int -> P (Probabilistic Double) -> Probabilistic Double
runP n r = expectedValue <$> gibbs n r

getProb :: Int -> ProbProg (Probabilistic Bool) -> Probabilistic Double
getProb n phi = (/) <$> runP n (runPP phi indicator)
                <*> runP n (runPP phi (pure . fmap (const 1)))


--------------------------------------------------------------------------------
-- | Instances of representations

-- | CCCs
instance Closed TradI where
  app m n = TradI $ runTradI m (runTradI n)
  lam f = TradI $ runTradI . f . TradI

instance Closed (ProbI k) where
  app m n = ProbI $ \k -> app (runProbI m k) (runProbI n k)
  lam f = ProbI $ \k -> lam $ \x -> runProbI (f (ProbI $ \_ -> x)) k
  -- | This breaks the law for closed categories; but it encodes the
  -- λ-homomorphism described in the paper.

instance Cartesian TradI where
  unit = TradI ()
  pair m n = TradI (runTradI m, runTradI n)
  fst_ = TradI . fst . runTradI
  snd_ = TradI . snd . runTradI

instance Cartesian (ProbI k) where
  unit = ProbI $ \_ -> unit
  pair m n = ProbI $ \k -> pair (runProbI m k) (runProbI n k)
  fst_ m = ProbI $ fst_ . runProbI m
  snd_ m = ProbI $ snd_ . runProbI m

-- | Constants
instance Constant (Entity -> Bool) "dog" TradI where
  c = TradI dog'

instance Constant (Entity -> Bool) "cat" TradI where
  c = TradI cat'

instance Constant (Entity -> Bool) "happy" TradI where
  c = TradI happy'

instance Constant (Entity -> Entity -> Bool) "chase" TradI where
  c = TradI chase'

instance Constant (Entity -> Entity -> Bool) "catch" TradI where
  c = TradI catch'

instance InCtx a str k TradI => Constant a str (ProbI k) where
  c = ProbI $ π @a @str

-- | Logic
instance Heyting TradI where  
  phi /\ psi = TradI $ runTradI phi && runTradI psi
  phi \/ psi = TradI $ runTradI phi || runTradI psi
  phi --> psi = TradI $ not (runTradI phi) || runTradI psi
  true = TradI True
  false = TradI False

instance Heyting (ProbI k) where
  phi /\ psi = ProbI $ \k -> runProbI phi k /\ runProbI psi k
  phi \/ psi = ProbI $ \k -> runProbI phi k \/ runProbI psi k
  phi --> psi = ProbI $ \k -> runProbI phi k --> runProbI psi k
  true = ProbI $ const true
  false = ProbI $ const false

-- | Class of types for which some finite domain of quantification can be
-- determined
class Finite a where
  domain :: [a]

instance Finite () where
  domain = [()]

instance Finite Entity where
  domain = entities

instance (Finite a, Finite b) => Finite (a, b) where
  domain = [ (a, b) | a <- domain, b <- domain ]  

-- | Assuming there is such a domain...
instance Finite a => HOL a TradI where
  forall f = TradI $ all (runTradI f) domain
  exists f = TradI $ any (runTradI f) domain

instance HOL a TradI => HOL a (ProbI k) where
  forall f = ProbI $ forall . runProbI f
  exists f = ProbI $ exists . runProbI f

-- | Interpreting the Probability DSL
instance RealRepr (P (Probabilistic Double)) where
  type REAL (P (Probabilistic Double)) = Probabilistic Double
  type BOOL (P (Probabilistic Double)) = Probabilistic Bool
  type Distr (P (Probabilistic Double)) = Distribution Double
  integrate distr f = sample distr >>= f
  mult m n = do x <- m
                y <- n
                pure ((*) <$> x <*> y)              
  indicator = pure . fmap (\b -> if b then 1 else 0)
