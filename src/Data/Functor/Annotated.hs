{-# LANGUAGE DeriveFunctor, TypeFamilies, PatternSynonyms, RankNTypes #-}

module Data.Functor.Annotated
  ( -- * The @Annotated@ type family
    Annotated
    
    -- * Types
  , AnnotatedF(..)
    
    -- ** Patterns for hiding @Fix@
  , pattern Note

    -- * Utility functions
  , note
  , erase
  , foldMemo
    
  ) where

import Data.Functor.Foldable (Fix(..), fold, unfold)
import Data.Functor.Decomposed

-- | @AnnotatedF a f@ transforms the functor @f@ to add
-- @a@-valued annotations.

data AnnotatedF a f t
  = NoteF { note_  :: a
          , erase_ :: f t
          }
  deriving (Eq, Ord, Functor)

pattern Note a e = Fix (NoteF a e)

instance Decomposed (AnnotatedF a) where
  fmap1 f n = n { erase_ = f (erase_ n) }
  
-- | If @T@ is a type that is defined as a fixpoint, then
-- an @Annotated a T@ will be the type of "@T@s with an
-- @a@-valued annotation attached to each node".

type family   Annotated a f
type instance Annotated a (Fix f) = Fix (AnnotatedF a f)

-- | Extract the top-level annotation.

note :: Fix (AnnotatedF t f) -> t
note (Note t _) = t

-- | Discard the annotations.

erase :: Functor f => Fix (AnnotatedF t f) -> Fix f
erase = unfold (\(Note _ e) -> e)

-- | Fold a function over a data structure, storing the
-- sub-results as annotations.

foldMemo :: Functor f => (f t -> t) -> Fix f -> Fix (AnnotatedF t f)
foldMemo phi = fold phi'
  where
    phi' x = Note (phi $ fmap note x) x
