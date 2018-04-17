{-# LANGUAGE DeriveFunctor #-}
module PrettyExpr where

import Types
import GetIso

type Prec = Integer

minPrec, maxPrec :: Prec
(minPrec, maxPrec) = (0, 5)

data PrTree ap un bin l a
  = PTLeaf l
  | PTApp ap a
  | PTUn Prec un a
  | PTBin Prec bin a a
  deriving (Eq, Ord, Show)

data WithPrec a = WP { wpPrec :: Prec, wpVal :: a }
  deriving (Eq, Ord, Show, Functor)

type PrTreeS = PrTree String String String String

typeToPrTreeS :: Type -> Cofree PrTreeS ()
typeToPrTreeS (TVar v) = () :< PTLeaf v
typeToPrTreeS Zero = () :< PTLeaf "0"
typeToPrTreeS One = () :< PTLeaf "1"

-- TODO fix these precedences
typeToPrTreeS (Sum a b) = () :< PTBin 3 "+" (typeToPrTreeS a) (typeToPrTreeS b)
typeToPrTreeS (Prod a b) = () :< PTBin 3 "*" (typeToPrTreeS a) (typeToPrTreeS b)
typeToPrTreeS (TIso a b) = () :< PTBin 3 "<->" (typeToPrTreeS a) (typeToPrTreeS b)

toPrTreeS :: GetIso i => Cofree (FExprS i) a -> Cofree PrTreeS a
toPrTreeS (x :< EVar v) = x :< PTLeaf v
toPrTreeS (x :< EIso i) = x :< PTLeaf (getIso i)
toPrTreeS (x :< EId) = x :< PTLeaf "id"
toPrTreeS (x :< ESym a) = x :< PTApp "sym" (toPrTreeS a)

-- TODO fix these precedences
toPrTreeS (x :< ECompose a b) = x :< PTBin 3 "|>" (toPrTreeS a) (toPrTreeS b)
toPrTreeS (x :< EProd a b) = x :< PTBin 3 "*" (toPrTreeS a) (toPrTreeS b)
toPrTreeS (x :< ESum a b) = x :< PTBin 3 "+" (toPrTreeS a) (toPrTreeS b)

prTreeToString :: Cofree PrTreeS a -> WithPrec String
prTreeToString (_ :< PTLeaf l) = WP maxPrec l
prTreeToString (_ :< PTApp ap t) = WP 4 (ap ++ " " ++ brk 4 (prTreeToString t))
prTreeToString (_ :< PTUn p' un t) = WP p' (un ++ sub p' t)
prTreeToString (_ :< PTBin p' bin a b) = WP p' (f1 a' b') where
  f1 sa sb = sa ++ " " ++ bin ++ " " ++ sb
  a' = sub p' a
  b' = sub p' b

sub :: Prec -> Cofree PrTreeS a -> String
sub p0 t = brk p0 (prTreeToString t)

brk p0 (WP p1 s) | p1 > p0 = s
                 | otherwise = "(" ++ s ++ ")"

prettyExpr :: GetIso i => Cofree (FExprS i) a -> String
prettyExpr = wpVal . prTreeToString . toPrTreeS

prettyType :: Type -> String
prettyType = wpVal . prTreeToString . typeToPrTreeS

prettyIType :: IType -> String
prettyIType (ITVar v) = v
prettyIType (ITIso a b) = prettyType a ++ " <-> " ++ prettyType b

