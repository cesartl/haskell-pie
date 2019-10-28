{-# LANGUAGE StandaloneDeriving #-}

module Pie.Nat(Nat, add1, zero, fromInt, toInt, iterNat) where

import Data.Functor.Foldable (cata, unfix, Fix(..), para)

data NatF r = SuccF r | ZeroF
            deriving (Eq, Show)

type Nat = Fix NatF

instance Functor NatF where
  fmap f (SuccF r) = SuccF (f r)
  fmap _ ZeroF = ZeroF

add1 :: Nat -> Nat
add1 n = Fix (SuccF n)

zero :: Nat
zero = Fix ZeroF

fromInt :: Int -> Nat
fromInt i = foldl (\n _ -> add1 n) zero [1..i]

toInt :: Nat -> Int
toInt (Fix ZeroF) = 0
toInt (Fix (SuccF n_1)) = 1 + toInt n_1

cata' :: Functor f => (f a -> a) -> Fix f -> a
cata' algebra fixed =
  let
    unfixed = unfix fixed -- :: (Fix f) becomes  f (Fix f)
    cata_algebra = cata' algebra -- :: Fix f -> a
    fa = fmap cata_algebra unfixed -- :: f a
  in algebra fa


iterFalgebra :: r -> (r -> r) -> NatF r -> r
iterFalgebra base step  ZeroF = base
iterFalgebra base step (SuccF r) = step r

iterNat :: Nat -> r -> (r -> r) -> r
iterNat n base step = cata' (iterFalgebra base step) n