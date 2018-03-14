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
, PIso(..)

, FExpr(..)
, FExprS
, NFExpr
, PFExpr
, Expr
, PExpr

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

data Iso = ZeroE      -- 0 + b <-> b
         | SwapS      -- a + b <-> b + a
         | AssocLS    -- a + (b + c) <-> (a + b) + c
         | UnitE      -- 1 * b <-> b
         | SwapP      -- a * b <-> b * a
         | AssocLP    -- a * (b * c) <-> (a * b) * c
         | Distrib0   -- 0 * b <-> 0
         | Distrib    -- (a + b) * c <-> (a * c) + (b * c)
  deriving (Eq, Ord, Show)

data PIso = PZeroE    -- 0 + b <-> b
          | PSwapS    -- a + b <-> b + a
          | PAssocLS  -- a + (b + c) <-> (a + b) + c
          | PUnitE2   -- 1 * 1 <-> 1
          | PSwapP    -- a * b <-> b * a
          | PAssocLP  -- a * (b * c) <-> (a * b) * c
          | PDistrib0 -- 0 * b <-> 0
          | PDistrib1 -- (a + (1 * b)) * c <-> (a * c) + ((1 * b) * c)
  deriving (Eq, Ord, Show)

data FExpr v i a = EVar v
                 | EIso i
                 | EId
                 | ESym a
                 | ECompose a a
                 | ESum a a
                 | EProd a a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type FExprS = FExpr String

type NFExpr = FExprS Iso
type PFExpr = FExprS PIso

type Expr = Cofree NFExpr ()
type PExpr = Cofree PFExpr ()

