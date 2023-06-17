{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
module Jaskell (push, liftS, liftS2, pushM, popM, liftSM) where

import Control.Arrow (Kleisli(Kleisli))

push :: a -> s -> (s, a)
push x s = (s, x)

liftS :: (a -> b) -> (s, a) -> (s, b)
liftS f (s, x) = (s, f x)

liftS2 :: (a -> b -> c) -> ((s, a), b) -> (s, c)
liftS2 f ((s, x), y) = (s, f x y)

pushM :: Functor m => m a -> Kleisli m s (s, a)
pushM mx = Kleisli \s -> fmap (s, ) mx

popM :: Functor m => (a -> m ()) -> Kleisli m (s, a) s
popM f = Kleisli \(s, x) -> fmap (const s) (f x)

liftSM :: Functor m => (a -> m b) -> Kleisli m (s, a) (s, b)
liftSM f = Kleisli \(s, x) -> fmap (s, ) (f x)