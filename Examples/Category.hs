{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

import Prelude hiding ((.), id)
import Control.Concurrent

{- Categories satsify the "typed" monoid laws:
   id . f = f = f . id
   f . (g . h) = (f . g) . h
   where f, g and h have compatible types
-}
class Category morphism where
  id :: morphism a a
  (.) :: morphism b c -> morphism a b -> morphism a c

-- regular functions are categories
instance Category (->) where
  id = \x -> x
  f . g = \x -> f (g x)

-- Kleisli categories have morphisms on the
-- form (a -> f b) for some functor f
newtype Kleisli f a b =
  K { run :: a -> f b }

-- Partial functions have f = Maybe
instance Category (Kleisli Maybe) where
  id = K Just
  K f . K g = K (\x -> case g x of
                    Just a -> f a
                    _ -> Nothing )

-- Async functions have f = Callback
newtype Callback b = Callback {
  onValue :: (b -> IO ()) -> IO ()
}
instance Category (Kleisli Callback) where
  id = K (\a -> Callback (\resolve -> resolve a))
  K f . K g = K (\a -> Callback (\resolve ->
    onValue (g a) (\b -> onValue (f b) resolve)))

-- Pure state functions implicitly pass state
-- along the composition
newtype Stateful s a b = Stateful {
  runStateful :: (a, s) -> (b, s)
  }

instance Category (Stateful s) where
  id = Stateful id
  Stateful f . Stateful g = Stateful (f . g)

-- stateful functions can be written on the form
-- a -> f b via currying, so the are kleisli arrows
-- with f a = s -> (a, s)

--------------------------------------------------------------------------------
-- Examples

father :: String -> String
father "Philip" = "Sven-Olof"
father "Sven-Olof" = "Erik"

grandfather = father . father

-- problem, father is partial and will crash
-- on input it doesn't know how to handle

type Partial = Kleisli Maybe

pFather :: Partial String String
pFather = K (\case
  "Philip" -> Just "Sven-Olof"
  "Sven-Olof" -> Just "Erik"
  _ -> Nothing)

-- composition now safely returns
-- Nothing when appropriate
pGrandFather = pFather . pFather

testPartial = run pGrandFather "Philip"
-- >> Just "Erik"

----------------------------------------------------------------------

type AsyncFunction = Kleisli Callback

-- We'll use the unsafe father for simplicity
fatherBackend = father

fatherAPI :: AsyncFunction String String
fatherAPI = K (\a -> Callback (\resolve -> do
    putStrLn "Calling api"
    threadDelay (1000 * 1000) -- simulate delay
    resolve (father a)
  ))

grandFatherAPI = fatherAPI . fatherAPI

testAsync =
  onValue (run grandFatherAPI "Philip") putStrLn

----------------------------------------------------------------------

fatherWithCount = Stateful (\(str, count) ->
  (father str, count + 1)) -- we'll also use the unsafe `father` here

grandFatherWithCount =
  fatherWithCount . fatherWithCount

testStateful =
  runStateful grandFatherWithCount ("Philip", 0)

----------------------------------------------------------------------

-- "Proof" that all Kleisli arrows form monads

-- Unfortunately needs some boilerplate wrapping
newtype Wrap f a = Wrap { unwrap :: f a }

instance Category (Kleisli f) => Monad (Wrap f) where
  return = Wrap . run id
  Wrap m >>= f =
    (Wrap . (\x -> x ()) . run) (K (unwrap . f) . K (const m))

-- Boilerplate instances
instance Category (Kleisli f) => Applicative (Wrap f) where
instance Category (Kleisli f) => Functor (Wrap f) where
