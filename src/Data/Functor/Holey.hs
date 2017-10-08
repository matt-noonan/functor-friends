{-# LANGUAGE DeriveFunctor, TypeFamilies, PatternSynonyms, RankNTypes #-}

module Data.Functor.Holey
  ( -- * The @Holey@ type family
    Holey
    
    -- * Types
  , HoleF(..)
    
    -- ** Patterns for hiding @Fix@
  , pattern Hole
  , pattern Existing

    -- * Utility functions
  , whole
  , plug
  , punch
    
  ) where

import Data.Functor.Foldable (Fix(..), unfix, fold, unfold)
import Data.Functor.Decomposed

-- | @HoleF f@ transforms the functor @f@ to add "holes".

data HoleF f t
  = HoleF           -- ^ A hole.
  | ExistingF (f t) -- ^ A normal value.
  deriving (Eq, Ord, Functor)

pattern Hole = Fix HoleF
pattern Existing e = Fix (ExistingF e)

instance Decomposed HoleF where
  fmap1 _ HoleF = HoleF
  fmap1 f (ExistingF e) = ExistingF (f e)

-- | If @T@ is a type that is defined as a fixpoint, then
-- a @Holey T@ will be the type of "@T@s with holes".

type family   Holey f
type instance Holey (Fix f) = Fix (HoleF f)

-- | Upgrade a value of type @T@ to a value of type
-- @T@-with-holes, without actually introducing any holes.

whole :: Functor f => Fix f -> Fix (HoleF f)
whole = unfold (ExistingF . unfix)

-- | Fill all holes in a data structure with the given value.

plug :: Functor f => Fix f -> Fix (HoleF f) -> Fix f
plug x = fold phi
  where
    phi HoleF = x
    phi (ExistingF e) = Fix e
  
-- | Replace any substructure matching the predicate with a hole.

punch :: Functor f => (Fix f -> Bool) -> Fix f -> Fix (HoleF f)
punch test = unfold phi
  where
    phi x@(Fix e) = if test x then HoleF else ExistingF e
