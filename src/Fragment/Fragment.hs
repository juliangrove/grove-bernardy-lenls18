{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Fragment.Fragment where

import ProbProg.ProbLang (Distribution(..), Probabilistic)
import Representations.Eval.Eval
import Representations.Eval.Model
import Representations.Representations


--------------------------------------------------------------------------------
-- | Examples

-- | Factor the result by 1 or 0, as according to whether b is 'True' or 'False'
observe' :: Probabilistic Bool -> ProbProg ()
observe' b = PP $ \k -> observe b (k ())

-- | Probabilistic program representing a normal distribution
normal :: Probabilistic Double
       -> Probabilistic Double
       -> ProbProg (Probabilistic Double)
normal mu sigma = PP $ integrate (Gaussian mu sigma)

-- | HOL representation of 'someone is tall'
someone_is_tall :: ProbI ((Entity -> Double, AsType "height"),
                          ((Entity -> Bool, AsType "person"),
                           ((Double -> Double -> Bool, AsType "≥"),
                            ((Double, AsType "theta_tall"),
                             ())))) Bool 
someone_is_tall
  = exists (lam $ \x -> app person x /\ app (app (≥) (app height x)) theta_tall)

-- | A probabilistic program returning contexts s.t. 'theta_tall' is interpreted
-- as a normal distribution with mean 72 and s.d. 3.
context_pp :: ProbProg
              (Probabilistic
               (Ctx TradI ((Entity -> Double, AsType "height"),
                           ((Entity -> Bool, AsType "person"),
                            ((Double -> Double -> Bool, AsType "≥"),
                             ((Double, AsType "theta_tall"),
                              ()))))))
context_pp = do d <- normal (pure 72) (pure 3)
                pure (pure (\d' -> AddCon (TradI height')
                                   (AddCon (TradI human')
                                    (AddCon (TradI (>=))
                                     (AddCon (TradI d')
                                      Empty)))) <*> d)

-- | Evaluate 'someone_is_tall' in the context of 'context_pp'
someone_is_tall_pp :: ProbProg (Probabilistic Bool)
someone_is_tall_pp = do κ <- context_pp
                        pure (runTradI . runProbI someone_is_tall <$> κ)
                        
-- >>> getProb 500000 someone_is_tall_pp
-- Prob {fromProb = 0.8422000000000148}

k0, k1 :: ProbProg
          (Probabilistic
            (Ctx TradI ((Entity, AsType "c"),
                        ((Entity, AsType "m"),
                         ((Entity, AsType "a"),
                          ((Entity, AsType "v"),
                           ((Entity -> Double, AsType "height"),
                            ((Double -> Double -> Bool, AsType "≥"),
                             ((Double, AsType "theta_tall"),
                              ())))))))))
k0 = do d <- normal (pure 68) (pure 3)
        pure (pure (\d' -> AddCon (TradI ca)
                           (AddCon (TradI m)
                            (AddCon (TradI a)
                             (AddCon (TradI v)
                              (AddCon (TradI height'')
                               (AddCon (TradI (>=))
                                (AddCon (TradI d')
                                 Empty))))))) <*> d)
k1 = do κ <- k0
        observe' (runTradI . runProbI
                  ((app (app (≥) (app height camilla)) theta_tall) --> false) <$> κ)
        observe' (runTradI . runProbI
                  ((app (app (≥) (app height matt)) theta_tall) --> false) <$> κ)
        observe' (runTradI . runProbI
                  (app (app (≥) (app height anna)) theta_tall) <$> κ)
        pure κ

-- | HOL representation of 'Vlad is tall'
vlad_is_tall :: ProbI ((Entity, AsType "c"),
                       ((Entity, AsType "m"),
                        ((Entity, AsType "a"),
                         ((Entity, AsType "v"),
                          ((Entity -> Double, AsType "height"),
                           ((Double -> Double -> Bool, AsType "≥"),
                            ((Double, AsType "theta_tall"),
                             ()))))))) Bool
vlad_is_tall
  = app (app (≥) (app height vlad)) theta_tall

-- | Evaluate 'vlad_is_tall' in the context of 'k0'
vlad_is_tall_k0 :: ProbProg (Probabilistic Bool)
vlad_is_tall_k0 = do κ <- k0
                     pure (runTradI . runProbI vlad_is_tall <$> κ)

-- >>> getProb 500000 vlad_is_tall_k0
-- Prob {fromProb = 0.5002679999999994}

-- | Evaluate 'vlad_is_tall' in the context of 'k1'
vlad_is_tall_k1 :: ProbProg (Probabilistic Bool)
vlad_is_tall_k1 = do κ <- k1
                     pure (runTradI . runProbI vlad_is_tall <$> κ)

-- >>> getProb 500000 vlad_is_tall_k1
-- Prob {fromProb = 0.24114865814765057}


--------------------------------------------------------------------------------
-- | Constants

-- | Degree stuff

theta_tall :: Constant Double "theta_tall" repr
           => repr Double
theta_tall = c @Double @"theta_tall"

(≥) :: Constant (Double -> Double -> Bool) "≥" repr
    => repr (Double -> Double -> Bool)
(≥) = c @(Double -> Double -> Bool) @"≥"


-- | Measures

height :: Constant (Entity -> Double) "height" repr
       => repr (Entity -> Double)
height = c @(Entity -> Double) @"height"


-- | One-place predicates

dog :: Constant (Entity -> Bool) "dog" repr
    => repr (Entity -> Bool)
dog = c @(Entity -> Bool) @"dog"

cat :: Constant (Entity -> Bool) "cat" repr
    => repr (Entity -> Bool)
cat = c @(Entity -> Bool) @"cat"

ginger_ale :: Constant (Entity -> Bool) "ginger-ale" repr
           => repr (Entity -> Bool)
ginger_ale = c @(Entity -> Bool) @"ginger-ale"

good :: Constant (Entity -> Bool) "good" repr
     => repr (Entity -> Bool)
good = c @(Entity -> Bool) @"good"

happy :: Constant (Entity -> Bool) "happy" repr
      => repr (Entity -> Bool)
happy = c @(Entity -> Bool) @"happy"

person :: Constant (Entity -> Bool) "person" repr
       => repr (Entity -> Bool)
person = c @(Entity -> Bool) @"person"


-- | Two-place predicates

chase :: Constant (Entity -> Entity -> Bool) "chase" repr
      => repr (Entity -> Entity -> Bool)
chase = c @(Entity -> Entity -> Bool) @"chase"

catch :: Constant (Entity -> Entity -> Bool) "catch" repr
      => repr (Entity -> Entity -> Bool)
catch = c @(Entity -> Entity -> Bool) @"catch"

drank :: Constant (Entity -> Entity -> Bool) "drank" repr
      => repr (Entity -> Entity -> Bool)
drank = c @(Entity -> Entity -> Bool) @"drank"


-- | Names and definite descriptions

camilla :: Constant Entity "c" repr
        => repr Entity
camilla = c @Entity @"c"

matt :: Constant Entity "m" repr
     => repr Entity
matt = c @Entity @"m"

anna :: Constant Entity "a" repr
     => repr Entity
anna = c @Entity @"a"

vlad :: Constant Entity "v" repr
     => repr Entity
vlad = c @Entity @"v"
