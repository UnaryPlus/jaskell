{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
module Jaskell 
  ( push, liftS, liftS2, pushM, popM, liftSM
  , run, runOn, runK, runKOn
  ) where

import Control.Arrow (Arrow, arr, Kleisli(Kleisli))

push :: Arrow arr => a -> arr s (s, a)
push x = arr (, x)

liftS :: Arrow arr => (a -> b) -> arr (s, a) (s, b)
liftS f = arr (fmap f)

liftS2 :: Arrow arr => (a -> b -> c) -> arr ((s, a), b) (s, c)
liftS2 f = arr \((s, x), y) -> (s, f x y)

pushM :: Functor m => m a -> Kleisli m s (s, a)
pushM mx = Kleisli \s -> fmap (s, ) mx

popM :: Functor m => (a -> m ()) -> Kleisli m (s, a) s
popM f = Kleisli \(s, x) -> fmap (const s) (f x)

liftSM :: Functor m => (a -> m b) -> Kleisli m (s, a) (s, b)
liftSM f = Kleisli \(s, x) -> fmap (s, ) (f x)

-- TODO: change names?
run :: (() -> t) -> t
run f = f ()

runOn :: s -> (s -> t) -> t
runOn s f = f s

runK :: Kleisli m () t -> m t
runK (Kleisli f) = f ()

runKOn :: s -> Kleisli m s t -> m t
runKOn s (Kleisli f) = f s