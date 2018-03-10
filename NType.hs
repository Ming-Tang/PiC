{-# LANGUAGE ViewPatterns #-}
module NType where
import Data.List
import Data.Maybe
import qualified Data.Traversable as T

import qualified Data.List.NonEmpty as NE

import Types

data NType = NVar String | NZero | NOnes Integer
           | NProd (NonEmpty NType) | NSum (NonEmpty NType)
           | NIso Type Type
  deriving (Eq, Ord, Show)

data NTFold m a = NTF { ntf0 :: m a
                      , ntf1 :: Integer -> m a
                      , ntfSum :: [a] -> m a
                      , ntfProd :: [a] -> m a
                      , ntfS :: String -> m a }

collect :: (Type -> Maybe [Type]) -> Type -> Maybe [Type]
collect f = collect' where
  collect' :: Type -> Maybe [Type]
  collect' (f -> Nothing) = Nothing
  collect' (f -> Just ts) = Just $ concatMap (\x -> fromMaybe [x] $ collect' x) ts

collectNE :: (Type -> Maybe [Type]) -> Type -> NonEmpty Type
collectNE f = NE.fromList . fromJust . collect f

toNType :: Type -> NType

toNType (TIso a b) = NIso a b
toNType (TVar s) = NVar s
toNType Zero = NZero
toNType One = NOnes 1

toNType s@(Sum _ _) = nsum $ collectNE getSum s where
  getSum (Sum a b) = Just [a, b]
  getSum _ = Nothing

  nsum :: NonEmpty Type -> NType
  nsum xs | all isOne xs = NOnes $ fromIntegral $ NE.length xs
          | otherwise = NSum $ NE.map toNType xs

  isOne One = True
  isOne _a = False

toNType s@(Prod _ _) = NProd $ NE.map toNType $ collectNE getProd s where
  getProd (Prod a b) = Just [a, b]
  getProd _ = Nothing

minOnes, maxOnes :: NTFold Identity Integer
invOnes :: NTFold Maybe Integer
minOnes = NTF (return 0) return (return . minimum) (return . sum) error
maxOnes = NTF (return 0) return (return . maximum) (return . sum) error
invOnes = NTF (return 0) return invSum (return . sum) error where
  invSum xs = case nub xs of { [x] -> return x; _ -> Nothing }

ntFold :: Monad m => NTFold m a -> NType -> m a

ntFold NTF { ntfS = f } (NVar s) = f s
ntFold NTF { ntf0 = z } NZero = z
ntFold NTF { ntf1 = f } (NOnes n) = f n
ntFold ntf@NTF { ntfSum = fs } (NSum (NE.toList -> ps)) = fs =<< mapM (ntFold ntf) ps
ntFold ntf@NTF { ntfProd = fp } (NProd (NE.toList -> ps)) = fp =<< mapM (ntFold ntf) ps
ntFold _ _ = undefined

