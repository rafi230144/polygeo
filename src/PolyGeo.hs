{-# LANGUAGE Haskell2010
  , BangPatterns
  , DataKinds
  , DeriveFunctor
  , DerivingStrategies
  , GADTs
  , GeneralizedNewtypeDeriving
  , InstanceSigs
  , LambdaCase
  , PolyKinds
  , ScopedTypeVariables
  , StandaloneDeriving
  , TypeApplications
#-}

{-# OPTIONS_GHC -Wall #-}

{- | Given Natural n,
    summing the products of
    an n-variate polynomial
    and an n-variate exponential
    over all n-tuples of Naturals
-}
module PolyGeo
  ( -- * Type-level Natural numbers
    N
      ( S
      , Z
      )
    -- * Variable-length tuples
  , Tup
      ( Nil
      , Cons
      )
  , zipWithTup
  -- * Multivariate polynomials
  , Poly
      ( Poly
      , unPoly
      )
  , fromListPoly
  , toListPoly
  , evalPoly
  -- * Multivariate exponentials
  , Geo
      ( Geo
      , unGeo
      )
  , evalGeo
  -- * Summation
  , sumPolyGeo
  ) where


-- + Imports

-- ++ From base:

import Data.Kind
  ( Type )

import Numeric.Natural
  ( Natural )
  
import GHC.List
  ( build )

import Control.Monad
  ( (<=<) )

-- ++ From hashable:

import Data.Hashable
  ( Hashable
    ( hashWithSalt )
  )

-- ++ From unordered-containers:

import Data.HashMap.Strict
  ( HashMap )
  
import qualified Data.HashMap.Strict as HM
  ( toList
  , empty
  , insertWith
  )

import Data.HashSet
  ( HashSet )
  
import qualified Data.HashSet as HS
  ( fromList
  , toList
  )


-- _ Arithmetic

type Nat = Word

{- | Given argument @n@,
    returns @[ 0 .. n - 1 ]@ fold/build fusibly
    (and without undeflow if @n = 0@)
-}
range :: Nat -> [Nat]
range =
    let range_r = \ g b !n !n' -> case compare n n' of
            GT -> g n' $ range_r g b n (n' + 1)
            _  -> b
    in  \ n -> build $ \ g b -> range_r g b n 0

{- | Given argument @n@,
    returns @(-1) ^ n@,
    where the return type is generic
-}
sgn :: forall a. Num a => Nat -> a
sgn = \ n -> case n `rem` 2 of
    0 -> 1
    _ -> -1

{- | Given arguments @n0@, @n1@,
    returns the 'Num'-polymorphic binomial coefficient @choose n0 n1@,
    with intermediate results represented as Degs of 'Natural'
-}
choose :: forall a. Num a => Nat -> Nat -> a
choose = \ n0 n1 -> fromIntegral @Natural $
    foldr (\ i k !r ->
        k $ r * (fromIntegral $ n0 - i) `quot` (fromIntegral $ i + 1)
      ) id (range n1) 1


-- * Type-level Natural numbers

{- | Peano natural numbers\;
    intended to be erased at runtime
-}
data N where
    Z :: N
    S :: N -> N


-- * Variable-length tuples

{- | Given arguments @n@, @a@,
    returns the type of (strict) @n@-tuples of Degs of @a@\;
    the runtime representation is *not* efficient!
-}
data Tup :: N -> Type -> Type where
    Nil :: forall a. Tup Z a
    Cons :: forall (n :: N) a. !(Tup n a) -> !a -> Tup (S n) a

deriving stock instance forall (n :: N) a. Eq a => Eq (Tup n a)
deriving stock instance forall (n :: N). Functor (Tup n)

instance forall (n :: N). Foldable (Tup n) where
    foldMap :: forall a m. Monoid m => (a -> m) -> Tup n a -> m
    foldMap = \ f -> \case
        Nil        -> mempty
        Cons ta' a -> f a <> foldMap f ta' -- folds from the right, not left, for efficiency

{- | *Really* type-safe version of 'zipWith' -}
zipWithTup ::
    forall (n :: N) a0 a1 b.
    (a0 -> a1 -> b) -> Tup n a0 -> Tup n a1 -> Tup n b
zipWithTup = \ g -> \cases
    (Nil)          (Nil)          -> Nil
    (Cons ta0' a0) (Cons ta1' a1) -> Cons (zipWithTup g ta0' ta1') (g a0 a1)

instance forall (n :: N) a. (Eq a, Hashable a) => Hashable (Tup n a) where
    hashWithSalt :: Int -> Tup n a -> Int
    hashWithSalt = flip $ foldr (\ a k !n ->
        k $ hashWithSalt n a
      ) id


-- * Multivariate polynomials

-- ** Multivariate polynomials

{- | Given arguments @n@, @a@,
    returns the type of @n@-variate polynomials with coefficients in @a@,
    represented as 'HashMap's from @'Tup' n 'Nat'@ to @a@\;
    is a newtype---not a synonym---so that instances like
    @forall (n :: 'N'). 'Functor' ('Poly' n)@ may be declared as needed
-}
newtype Poly :: N -> Type -> Type where
    Poly :: forall (n :: N) a. { unPoly :: HashMap (Tup n Nat) a } -> Poly n a

{- | 'Poly's from lists of (degree, coefficient) pairs\;
    coefficients corresponding to the same degree are added
-}
fromListPoly :: forall (n :: N) a. Num a => [(Tup n Nat, a)] -> Poly n a
fromListPoly = \ sptna -> Poly $ foldr (\ (tn, a) k !f ->
    k $ HM.insertWith (+) tn a f
  ) id sptna HM.empty

{- | Lists of (degree, coefficient) pairs from 'Poly's -}
toListPoly :: forall (n :: N) a. Poly n a -> [(Tup n Nat, a)]
toListPoly = HM.toList . unPoly

{- | 'Poly' evaluation -}
evalPoly :: forall (n :: N) a. Num a => Poly n a -> Tup n a -> a
evalPoly = \ f ta -> sum $ do
    (tn, a) <- toListPoly f
    pure . (a *) . product $ zipWithTup (^) ta tn


-- *_ (Finite) sets of Degs of multivariate polynomials

{- | Given argument @n@,
    returns the type of sets of degrees of @n@-variate polynomials,
    represented as @'HashSet' ('Tup' n Nat)@s
-}
newtype Degs :: N -> Type where
    Degs :: forall (n :: N). { unDegs :: HashSet (Tup n Nat) } -> Degs n

{- | 'Degs' from list of degrees -}
fromListDegs :: forall (n :: N). [Tup n Nat] -> Degs n
fromListDegs = Degs . HS.fromList

{- | List of degrees from 'Deg's -}
toListDegs :: forall (n :: N). Degs n -> [Tup n Nat]
toListDegs = HS.toList . unDegs

{- | Downward set of degree -}
hull :: forall (n :: N). Tup n Nat -> Degs n
hull =
    let hull_r :: forall (n' :: N). Tup n' Nat -> [Tup n' Nat]
        hull_r = \case
            Nil        -> [Nil]
            Cons tn' n -> do
                n' <- [ 0 .. n ]
                (`Cons` n') <$> hull_r tn'
    in  fromListDegs . hull_r

{- | Downward closure of (unfiltered) support of a polynomial -}
suppHull :: forall (n :: N) a. Poly n a -> Degs n
suppHull = fromListDegs . (toListDegs . hull <=< (fst <$>) . toListPoly)


-- * Multivariate exponentials

{- | Given arguments @n@, @a@,
    returns the type of @n@-variate exponentials with bases in @a@,
    represented as @'Tup' n a@s;
    is a newtype---not a synonym---so that instances like
    @forall (n :: 'N'). 'Functor' ('Geo' n)@ may be declared as needed
-}
newtype Geo :: N -> Type -> Type where
    Geo :: forall (n :: N) a. { unGeo :: Tup n a } -> Geo n a

deriving newtype instance forall (n :: N). Functor (Geo n)

{- | 'Geo' evaluation -}
evalGeo :: forall (n :: N) a. Num a => Geo n a -> Tup n Nat -> a
evalGeo = fmap product . zipWithTup (^) . unGeo


-- * Summation

{- | Given natural n,
    summing the products of
    an n-variate polynomial
    and an n-variate exponential
    over all n-tuples of naturals\;
    e.g.,

>>> :set -XTypeApplications
>>> :set -Wall -Wno-name-shadowing

>>> let term0 = ( Nil `Cons` 3 , 1 )
>>> let poly = fromListPoly [ term0 ]
>>> let geo = Geo $ Nil `Cons` (1 / 2)
>>> sumPolyGeo @_ @Rational poly geo
26 % 1

>>> let term0 = ( Nil `Cons` 3 `Cons` 0 , 1)
>>> let term1 = ( Nil `Cons` 1 `Cons` 1 , -3)
>>> let term2 = ( Nil `Cons` 0 `Cons` 2 , 2)
>>> let poly = fromListPoly [ term0 , term1 , term2 ]
>>> let geo = Geo $ Nil `Cons` (-1 / 4) `Cons` (1 / 5)
>>> sumPolyGeo @_ @Rational poly geo
223 % 250
-}
sumPolyGeo :: forall (n :: N) a. Fractional a => Poly n a -> Geo n a -> a
sumPolyGeo = \ f g -> sum $ do
    d <- toListDegs $ suppHull f
    d' <- toListDegs $ hull d
    let !a0 = product . fmap sgn $ zipWithTup (-) d d'
        !a1 = product $ zipWithTup choose d d'
        !a2 = evalPoly f $ fmap fromIntegral d'
        !a3 = evalGeo g d
        !a4 = evalGeo (fmap (1 -) g) (fmap (1 +) d)
    pure $ a0 * a1 * a2 * a3 / a4
