{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
module Jaskell 
  ( -- * Stack utilities
    push, liftS, liftS2, pushM, popM, liftSM
    -- * Running programs
  , run, runOn, runK, runKOn
  ) where

import Control.Arrow (Arrow, arr, Kleisli(Kleisli))

-- | Push a value.
push :: Arrow arr => a -> arr s (s, a)
push x = arr (, x)

-- | Apply a function to the top value.
liftS :: Arrow arr => (a -> b) -> arr (s, a) (s, b)
liftS f = arr (fmap f)

-- | Apply a function to the top two values.
liftS2 :: Arrow arr => (a -> b -> c) -> arr ((s, a), b) (s, c)
liftS2 f = arr \((s, x), y) -> (s, f x y)

-- | Perform a monadic action and push the result.
pushM :: Functor m => m a -> Kleisli m s (s, a)
pushM mx = Kleisli \s -> fmap (s, ) mx

-- | Pop the top value and feed it to a monadic action.
popM :: Functor m => (a -> m ()) -> Kleisli m (s, a) s
popM f = Kleisli \(s, x) -> fmap (const s) (f x)

-- | Pop the top value, feed it to a monadic action, and push the result.
liftSM :: Functor m => (a -> m b) -> Kleisli m (s, a) (s, b)
liftSM f = Kleisli \(s, x) -> fmap (s, ) (f x)

-- | Run a direct Jaskell program on an empty stack.
run :: (() -> t) -> t
run f = f ()

-- | Run a direct Jaskell program on the given stack.
runOn :: s -> (s -> t) -> t
runOn s f = f s

-- | Run a monadic Jaskell program on an empty stack.
runK :: Kleisli m () t -> m t
runK (Kleisli f) = f ()

-- | Run a monadic Jaskell program on the given stack.
runKOn :: s -> Kleisli m s t -> m t
runKOn s (Kleisli f) = f s