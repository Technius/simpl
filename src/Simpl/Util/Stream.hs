{-# LANGUAGE DeriveFunctor #-}
{-|
Module      : Simpl.Util.Stream
Description : Infinite lists. API borrowed from Stream.

API borrowed from
https://hackage.haskell.org/package/Stream-0.4.7.2/docs/Data-Stream.html
-}
module Simpl.Util.Stream where

import Prelude hiding (iterate)

data Stream a = Cons a (Stream a)
  deriving (Functor)

head :: Stream a -> a
head (Cons x _) = x

tail :: Stream a -> Stream a
tail (Cons _ xs) = xs

-- | Generate a Stream from a starting state
unfold :: (b -> (a, b)) -> b -> Stream a
unfold f st = let (el, newSt) = f st in Cons el (unfold f newSt)

-- | Generate a Stream by successively applying a function
iterate :: (a -> a) -> a -> Stream a
iterate f x = Cons x (iterate f (f x))

-- | A Stream that repeats the same value
repeat :: a -> Stream a
repeat x = iterate (const x) x
