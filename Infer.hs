module Infer (IsoType, UnifyErr(..), typeTree, unify, subst) where
import Text.Show.Prettyprint
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Traversable as T

import Control.Monad.State.Strict
import Control.Comonad

import Types

infixr 3 =:=
infixr 4 <->

(=:=) :: Type -> Type -> Constraint
(=:=) = CEq

(<->) :: Type -> Type -> Type
(<->) = TIso

-- Code is almost completely based on:
-- https://brianmckenna.org/blog/type_annotation_cofree

data Constraint = CEq Type Type
  deriving (Eq, Ord, Show)

data TypeResult = TR { trConstraints :: [Constraint]
                     , trAssumptions :: Map String [Type] }
  deriving (Eq, Ord, Show)

instance Monoid TypeResult where
  mempty = TR mempty mempty
  mappend a b = TR (trConstraints a `mappend` trConstraints b)
                   (trAssumptions a `mappend` trAssumptions b)

newtype TypeState = TS { tsVarId :: Int }
  deriving (Eq, Ord, Show)

type TypeCheck = State TypeState

data UnifyErr = UnifyErr Type Type
  deriving (Eq, Ord, Show)

type TypeMap = Map String Type

class IsoType a where
  isoType :: a -> (Type, Type)

instance IsoType Iso where
  isoType ZeroE = (Sum Zero vB, vB)
  isoType SwapS = (Sum vB1 vB2, Sum vB2 vB1)
  isoType AssocLS = (Sum vB1 (Sum vB2 vB3), Sum (Sum vB1 vB2) vB3)
  isoType UnitE = (Prod One vB, vB)
  isoType SwapP = (Prod vB1 vB2, Prod vB2 vB1)
  isoType AssocLP = (Prod vB1 (Prod vB2 vB3), Prod (Prod vB1 vB2) vB3)
  isoType Distrib0 = (Prod Zero vB, Zero)
  isoType Distrib = (Prod (Sum vB1 vB2) vB3, Sum (Prod vB1 vB3) (Prod vB2 vB3))

instance IsoType PIso where
  isoType PZeroE = (Sum Zero vB, vB)
  isoType PSwapS = (Sum vB1 vB2, Sum vB2 vB1)
  isoType PAssocLS = (Sum vB1 (Sum vB2 vB3), Sum (Sum vB1 vB2) vB3)
  isoType PUnitE2 = (Prod One One, vB)
  isoType PSwapP = (Prod vB1 vB2, Prod vB2 vB1)
  isoType PAssocLP = (Prod vB1 (Prod vB2 vB3), Prod (Prod vB1 vB2) vB3)
  isoType PDistrib0 = (Prod Zero vB, Zero)
  isoType PDistrib1 = ( Prod (Sum vB1 (Prod One vB2)) vB3
                      , Sum (Prod vB1 vB3) (Prod vB2 vB3))

-------------------------------------------------------------------------------

freshVarId :: String -> TypeCheck Type
freshVarId prefix = do
  v <- tsVarId <$> get
  modify $ \s -> s { tsVarId = succ v }
  return $ TVar $ prefix ++ "." ++ show v

attribute :: IsoType i => Cofree (FExprS i) () -> Cofree (FExprS i) (Type, TypeResult)
attribute c = evalState (T.sequence $ extend genConstraints c) initial
  where initial = TS { tsVarId = 0 }

vB, vB1, vB2, vB3 :: Type
vB = TVar "b"
vB1 = TVar "a"
vB2 = TVar "b"
vB3 = TVar "c"

tr0 :: [Constraint] -> TypeResult
tr0 = flip TR mempty

freshVarABC :: TypeCheck (Type, Type, Type)
freshVarABCD :: TypeCheck (Type, Type, Type, Type)

freshVarABC = do
  a <- freshVarId "a"
  b <- freshVarId "b"
  c <- freshVarId "c"
  return (a, b, c)

freshVarABCD = do
  (a, b, c) <- freshVarABC
  d <- freshVarId "d"
  return (a, b, c, d)

genConstraints :: IsoType i => Cofree (FExprS i) () -> TypeCheck (Type, TypeResult)
genConstraints (() :< EVar s) = do
  var <- freshVarId $ "$v$" ++ s
  return (var, TR [] $ M.singleton s [var])

genConstraints (() :< EIso iso) = do
  let (tin, tout) = isoType iso
  let (vin, vout) = (allVars tin, allVars tout)
  let vars = S.toList $ S.fromList (vin ++ vout)
  subs <- M.fromList . zip vars <$> mapM freshVarId vars
  let (tin', tout') = (subst subs tin, subst subs tout)
  t <- freshVarId "$i"
  return (t, tr0 [ t =:= tin' <-> tout' ])

-- id :: a <-> a
genConstraints (() :< EId) = do
  t <- freshVarId "$id"
  a <- freshVarId "a"
  b <- freshVarId "b"
  return (t, tr0 [ t =:= a <-> a, a =:= b ])

-- f :: a <-> b
-- sym f :: b <-> a
genConstraints (() :< ESym f) = do
  (t', aTR) <- genConstraints f
  t <- freshVarId "$sym"
  a <- freshVarId "a"
  b <- freshVarId "b"
  return (t, tr0 [ t =:= b <-> a
                 , t' =:= a <-> b ]
             `mappend` aTR)

-- f :: a <-> c, g :: c <-> b
-- f >> g :: a <-> b
genConstraints (() :< ECompose f g) = do
  (f, fTR) <- genConstraints f
  (g, gTR) <- genConstraints g
  t <- freshVarId "$co"
  (a, b, c) <- freshVarABC
  return (t, fTR `mappend` gTR `mappend`
                 tr0 [ t =:= a <-> b
                     , f =:= a <-> c
                     , g =:= c <-> b ])

-- f :: a <-> b, g :: c <-> d
-- f + g :: a + c <-> b + d
genConstraints (() :< ESum f g) = do
  (f, fTR) <- genConstraints f
  (g, gTR) <- genConstraints g
  t <- freshVarId "$sum"
  (a, b, c, d) <- freshVarABCD
  return (t, fTR `mappend` gTR `mappend`
                 tr0 [ t =:= Sum a c <-> Sum b d
                     , f =:= a <-> b
                     , g =:= c <-> d ])

-- f :: a <-> b, g :: c <-> d
-- f * g :: a * c <-> b * d
genConstraints (() :< EProd f g) = do
  (f, fTR) <- genConstraints f
  (g, gTR) <- genConstraints g
  t <- freshVarId "$prod"
  (a, b, c, d) <- freshVarABCD
  return (t, fTR `mappend` gTR `mappend`
                 tr0 [ t =:= Prod a c <-> Prod b d
                     , f =:= a <-> b
                     , g =:= c <-> d ])

solveConstraints :: [Constraint] -> Either UnifyErr TypeMap
solveConstraints =
    foldl (\b a -> liftM2 mappend (solve b a) b) $ return M.empty
          where solve maybeSubs (CEq a b) = do
                  subs <- maybeSubs
                  let (a', b') = (subst subs a, subst subs b)
                  unify a' b'

unifyBin :: Type -> Type -> Type -> Type -> Either UnifyErr TypeMap
unifyBin a b c d = do
  s1 <- unify a c
  liftM2 mappend (unify (subst s1 b) (subst s1 d)) $ return s1

occurs :: String -> Type -> Bool
occurs i (TVar i') = i == i'
occurs _ Zero = False
occurs _ One = False
occurs i (Sum a b) = occurs i a || occurs i b
occurs i (Prod a b) = occurs i a || occurs i b
occurs i (TIso a b) = occurs i a || occurs i b

unify :: Type -> Type -> Either UnifyErr TypeMap
--unify (TVar i) b@(TVar i') = return $ M.singleton i b
unify (TVar i) b | not $ occurs i b = return $ M.singleton i b
unify a (TVar i) | not $ occurs i a = return $ M.singleton i a
unify Zero Zero = return M.empty
unify One One = return M.empty
unify (Sum a b) (Sum c d) = unifyBin a b c d
unify (Prod a b) (Prod c d) = unifyBin a b c d
unify (TIso a b) (TIso c d) = unifyBin a b c d
unify a b | a == b = Right M.empty
unify a b = Left (UnifyErr a b)

subst :: TypeMap -> Type -> Type
subst subs v@(TVar i) = maybe v (subst subs) $ M.lookup i subs
subst _ Zero = Zero
subst _ One = One
subst subs (Sum a b) = Sum (subst subs a) (subst subs b)
subst subs (Prod a b) = Prod (subst subs a) (subst subs b)
subst subs (TIso a b) = TIso (subst subs a) (subst subs b)

allVars :: Type -> [String]
allVars (TVar v) = [v]
allVars Zero = []
allVars One = []
allVars (Sum a b) = allVars a ++ allVars b
allVars (Prod a b) = allVars a ++ allVars b
allVars (TIso a b) = allVars a ++ allVars b

typeTree :: IsoType i => Cofree (FExprS i) () -> Either UnifyErr (Cofree (FExprS i) Type)
typeTree c =
    let result = attribute c
        (r :< _) = result
        maybeSubs = solveConstraints . trConstraints $ snd r
    in fmap (\subs -> fmap (subst subs . fst) result) maybeSubs

