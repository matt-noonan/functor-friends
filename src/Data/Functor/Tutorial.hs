{-# LANGUAGE LambdaCase, PatternSynonyms, DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Data.Functor.Tutorial where

import Data.Functor.Holey
import Data.Functor.Annotated
import Data.Functor.Decomposed

import Data.Functor.Foldable ( Fix(..), fold, cata )

-- | An example AST type, as an F-algebra. This AST will be used
-- to demonstrate some tricks for composing new data types
-- while maintaining separation of concerns.
--
-- This approach was inspired by the @recursion-schemes@ library
-- and the "Data Types a la Carte" paper.

data AstF t
  = RealF    Double
  | ComplexF Double Double
  | VarF String
  | AddF t t
  | MulF t t
  | AbsF t
  deriving (Eq, Ord, Functor)

type Ast = Fix AstF

-- | Patterns to simplify the construction and destructuring of ASTs
pattern Add x y = Fix (AddF x y)
pattern Mul x y = Fix (MulF x y)
pattern Abs x   = Fix (AbsF x)
pattern Real x  = Fix (RealF x)
pattern Complex x y = Fix (ComplexF x y)
pattern Var x   = Fix (VarF x)

-- | A pretty-printing typeclass.
--
-- This demonstrates an approach to separation of concerns; the
-- pretty-printer for normal ASTs is implemented independently from
-- the pretty-printer for holes and the pretty-printer for annotations.
--
-- Although the implementations don't know about each other, they can
-- still be composed; as a result, a @Holey Ast@ knows how to print itself
-- using a mixture of the pretty-printer for @Ast@s and the pretty-printer
-- for @Hole@s.

class Pretty a where
  pretty :: a -> String

-- | Pretty-printer for fixpoints.
instance Pretty (f (Fix f)) => Pretty (Fix f) where
  pretty (Fix x) = pretty x

-- | Pretty-printer for ASTs.
instance Pretty t => Pretty (AstF t) where
  
  pretty = \case
        RealF d        -> show d
        ComplexF re im -> paren (show re ++ " + " ++ show im ++ "i")
        AddF x y       -> paren (pretty x ++ " + " ++ pretty y)
        MulF x y       -> paren (pretty x ++ " * " ++ pretty y)
        AbsF x         -> "|" ++ pretty x ++ "|"
        VarF v         -> v
    where
      paren x = "(" ++ x ++ ")"

-- | Pretty-printer for holes.
instance Pretty (f (Fix (HoleF f))) => Pretty (HoleF f (Fix (HoleF f))) where

  pretty HoleF         = "_"
  pretty (ExistingF x) = pretty x

-- | Pretty-printer for annotations.
instance (Pretty (f (Fix (AnnotatedF a f))), Show a) => Pretty (AnnotatedF a f (Fix (AnnotatedF a f))) where

  pretty (NoteF ann x) = pretty x ++ "{" ++ show ann ++ "}"
  
-- | A simple expression
expr1 :: Ast
expr1 = Abs (Add (Mul (Real 2) (Complex 0 1)) (Real 1))

-- | Another simple expression
expr2 :: Ast
expr2 = Add (Mul (Var "z") (Var "z")) (Mul (Real 3) (Var "z"))

-- | An expression-with-holes. Holes can be plugged with 'plug'.
--
-- λ> pretty expr3
-- "|(_ + 1.0)|"
--
-- λ> pretty (plug (Var "z") expr3)
--"|(z + 1.0)|"
--
-- λ> :t plug (Var "z") expr3
-- plug (Var "z") expr3 :: Fix AstF -- aka Ast

expr3 :: Holey Ast
expr3 = Existing (AbsF (Existing (AddF Hole (Existing (RealF 1)))))

-- | Real and Complex type tags
data Type = R | C deriving (Eq, Show)

-- | Annotate each subexpression with its type (real or complex).
--
-- λ> pretty expr1
-- "|((2.0 * (0.0 + 1.0i)) + 1.0)|"
--
-- λ> pretty (inferTypes expr1)
-- "|((2.0{R} * (0.0 + 1.0i){C}){C} + 1.0{R}){C}|{R}"

inferTypes :: Ast -> Annotated Type Ast
inferTypes = foldMemo typeOf
  where
    typeOf = \case
      AddF R R -> R
      MulF R R -> R
      AbsF _   -> R
      RealF _  -> R
      _        -> C

-- | Remove all real-valued subexpressions, leaving a 'Hole'
-- in their place.
--
-- λ> pretty expr2
-- "((z * z) + (3.0 * z))"
--
-- λ> pretty (unreal expr2)
-- "((z * z) + (_ * z))"

unreal :: Ast -> Holey Ast
unreal = eraseTypes . punch (\x -> note x == R) . inferTypes
  where
   eraseTypes = natmap (fmap1 erase_)

