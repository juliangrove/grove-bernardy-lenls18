{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Fragment.Fragment where

import Representations.Representations
import Representations.Eval.Model
import Representations.Eval.Eval
-- import Representations.Bayesian.Logits
-- import Representations.Bayesian.MCMC


-- test

-- | Normal distribution
-- normal :: Probabilistic Double
        -- -> Probabilistic Double
        -- -> P (Probabilistic Double)
-- normal mu sigma = P $ Sample (Gaussian mu sigma)

-- | "K" from paper
-- context :: P (Probabilistic
              -- (Ctx TradI ((Entity -> Double, AsType "height"),
                          -- ((Entity -> Bool, AsType "person"),
                           -- ((Double -> Double -> Bool, AsType "≥"),
                            -- ((Double, AsType "theta_tall"),
                             -- ()))))))
-- context = normal (pure 72) (pure 3)
  -- >>= \d -> pure (pure (\d' -> AddCon (TradI height')
                         -- (AddCon (TradI human')
                          -- (AddCon (TradI (>=))
                           -- (AddCon (TradI d')
                            -- Empty)))) <*> d)

-- | 'someone is tall'
-- ex :: ProbI ((Entity -> Double, AsType "height"),
              -- ((Entity -> Bool, AsType "person"),
                -- ((Double -> Double -> Bool, AsType "≥"),
                  -- ((Double, AsType "theta_tall"),
                    -- ())))) Bool 
-- ex = exists (lam $ \x -> app person x /\ app (app (≥) (app height x)) theta_tall)

-- | Evaluating 'someone is tall' in the context
-- program :: P (Probabilistic Bool)
-- program = fmap (fmap runTradI) $ context
  -- >>= \k -> pure (pure (runProbI ex) <*> k)


-- run :: Int -> P (Probabilistic Bool) -> IO Prob
-- run n phi = do
  -- phi' <- mcmc n phi
  -- return (trueProb phi')

-- >>> run 10000 program'
-- 0.8976999999999999( large )


-- | Degrees

theta_tall :: Constant Double "theta_tall" repr
           => repr Double
theta_tall = c @Double @"theta_tall"

(≥) :: Constant (Double -> Double -> Bool) "≥" repr
    => repr (Double -> Double -> Bool)
(≥) = c @(Double -> Double -> Bool) @"≥"

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

amy :: Constant Entity "amy" repr
    => repr Entity
amy = c @Entity @"amy"

ashley :: Constant Entity "ashley" repr
       => repr Entity
ashley = c @Entity @"ashley"

emacs :: Constant Entity "emacs" repr
      => repr Entity
emacs = c @Entity @"emacs"

matt :: Constant Entity "matt" repr
     => repr Entity
matt = c @Entity @"matt"

jean_philippe :: Constant Entity "jean-philippe" repr
              => repr Entity
jean_philippe = c @Entity @"jean-philippe"

julian :: Constant Entity "julian" repr
       => repr Entity
julian = c @Entity @"julian"

stergios :: Constant Entity "stergios" repr
         => repr Entity
stergios = c @Entity @"stergios"
