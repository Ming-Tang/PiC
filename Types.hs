{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Types (
  Map
, Set
, NonEmpty(..)
, Cofree(..)
, Identity
, Type(..)
, Iso(..)
, FExpr(..)
, Expr
) where
import Data.Map(Map)
import Data.Set(Set)

import Data.List.NonEmpty(NonEmpty(..))
import Data.Functor.Identity

--import Control.Applicative
--import Control.Monad
--import Control.Monad.State.Strict
--import Control.Comonad
import Control.Comonad.Cofree

data Type = TVar String
          | Zero
          | One
          | Sum Type Type
          | Prod Type Type
          | TIso Type Type -- TODO separate type for Iso
  deriving (Eq, Ord, Show)

data Iso = ZeroE
         | SwapS
         | AssocLS
         | Unite
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

