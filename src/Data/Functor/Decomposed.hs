{-# LANGUAGE RankNTypes #-}

module Data.Functor.Decomposed
  ( Decomposed(..)
  , natmap
  ) where

import Data.Functor.Foldable (Fix(..), unfix, unfold)

-- | A type class for types @d :: (* -> *) -> * -> *@ that
-- represent compositions of functors. For a functor @f :: * -> *@,
-- @d f t@ should look like some functor applied to the type @f t@,
-- and @fmap1@ is just @fmap@ for that functor.
class Decomposed d where
  fmap1 :: (f t -> g t) -> d f t -> d g t

-- | Given a natural transformation between functors, produce a
-- function between the fixpoints of those functors.
natmap :: (Functor f, Functor g)
       => (forall t. f t -> g t)
       -> Fix f
       -> Fix g
natmap f = unfold (f . unfix)
