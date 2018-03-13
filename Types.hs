{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Types (
  Map
, Set
, NonEmpty(..)
, Cofree(..)
, Identity
, Type(..)
, FType(..)
, Iso(..)
, FExpr(..)
, Expr
) where
import Data.Map(Map)
import Data.Set(Set)

import Data.List.NonEmpty(NonEmpty(..))
import Data.Functor.Identity

import Control.Comonad.Cofree

data Type = TVar String
          | Zero
          | One
          | Sum Type Type
          | Prod Type Type
          | TIso Type Type -- TODO separate type for Iso
  deriving (Eq, Ord, Show)

data FType a = FZero
             | FOne
             | FSum a a
             | FProd a a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Iso = ZeroE
         | SwapS
         | AssocLS
         | UnitE
         | SwapP
         | AssocLP
         | Distrib0
         | Distrib
  deriving (Eq, Ord, Show)

data FExpr a = EVar String
             | EIso Iso
             | EId
             | ESym a
             | ECompose a a
             | ESum a a
             | EProd a a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type Expr = Cofree FExpr ()

