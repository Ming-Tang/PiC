module FType (WiresSizes(..), fold, annotate, zipWithFType, wires, sizes, wiresSizes, toFType) where
import Data.Maybe(fromJust)
import qualified Data.Set as S
import qualified Data.Monoid as M
import qualified Data.Traversable as T

import Control.Monad.State.Strict
import Control.Comonad

import Types

data WiresSizes = WS { wsBeginIdx :: Integer, wsSize :: Integer, wsArities :: [Integer] }
  deriving (Eq, Ord, Show)

fold :: (a, a, a -> a -> a, a -> a -> a)
        -> Cofree FType b -> Cofree FType a
fold (z, _, _, _) (_ :< FZero) = z :< FZero
fold (_, o, _, _) (_ :< FOne) = o :< FOne
fold f@(_, _, s, _) (_ :< FSum a b) = s a' b' :< FSum a1 b1 where
  a1@(a' :< _) = fold f a
  b1@(b' :< _) = fold f b
fold f@(_, _, _, p) (_ :< FProd a b) = p a' b' :< FProd a1 b1 where
  a1@(a' :< _) = fold f a
  b1@(b' :< _) = fold f b

sizes :: Cofree FType a -> Cofree FType [Integer]
sizes = fmap S.toList . fold (S.singleton 0, S.singleton 1 , S.union, adds) where
    adds a b = S.fromList ((+) <$> S.toList a <*> S.toList b)

annotate :: Monoid w => (w, w) -> w -> Cofree FType a -> Cofree FType (w, w)
annotate (dw0, _) w (_ :< FZero) = (w, dw0) :< FZero
annotate (_, dw1) w (_ :< FOne) = (w, dw1) :< FOne
annotate d w (_ :< FSum a b) = annotate2 d w FSum a b
annotate d w (_ :< FProd a b) = annotate2 d w FProd a b

annotate2 d w f a b =
  let a'@((_, da) :< _) = annotate d w a
      w' = w `mappend` da
      b'@((_, db) :< _) = annotate d w' b in
  ((w, da `mappend` db) :< f a' b')

wires :: Cofree FType a -> Cofree FType (Integer, Integer)
wires = fmap (\(M.Sum a, M.Sum b) -> (a, b))
             . annotate (M.Sum 0, M.Sum 1) (M.Sum 0)

zipWithFType :: (Show a, Show b) => (a -> b -> c) -> Cofree FType a -> Cofree FType b -> Maybe (Cofree FType c)
zipWithFType f (a :< FZero) (b :< FZero) = return (f a b :< FZero)
zipWithFType f (a :< FOne) (b :< FOne) = return (f a b :< FOne)
zipWithFType f (a :< FSum x1 y1) (b :< FSum x2 y2) =
  do x' <- zipWithFType f x1 x2
     y' <- zipWithFType f y1 y2
     return (f a b :< FSum x' y')

zipWithFType f (a :< FProd x1 y1) (b :< FProd x2 y2) =
  do x' <- zipWithFType f x1 x2
     y' <- zipWithFType f y1 y2
     return (f a b :< FProd x' y')

zipWithFType _ x y = Nothing

wiresSizes :: Cofree FType a -> Cofree FType WiresSizes
wiresSizes t = fromJust $ zipWithFType (\(a, b) c -> WS a b c) (wires t) (sizes t)

toFType :: Type -> Maybe (Cofree FType ())
toFType Zero = return (() :< FZero)
toFType One = return (() :< FOne)
toFType (Sum a b) = (\a b -> () :< FSum a b) <$> toFType a <*> toFType b
toFType (Prod a b) = (\a b -> () :< FProd a b) <$> toFType a <*> toFType b
toFType (TVar _) = Nothing
toFType (TIso _ _) = Nothing

