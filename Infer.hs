module Infer (IsoType, UnifyErr(..), typeTree, getType, unify, subst) where
import Text.Show.Prettyprint
import Debug.Trace
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

class Show a => IsoType a where
  isoType :: a -> (Type, Type)

instance IsoType Iso where
  isoType I1 = (One, One)
  isoType ZeroE = (Sum Zero vB, vB)
  isoType SwapS = (Sum vB1 vB2, Sum vB2 vB1)
  isoType AssocLS = (Sum vB1 (Sum vB2 vB3), Sum (Sum vB1 vB2) vB3)
  isoType UnitE = (Prod One vB, vB)
  isoType SwapP = (Prod vB1 vB2, Prod vB2 vB1)
  isoType AssocLP = (Prod vB1 (Prod vB2 vB3), Prod (Prod vB1 vB2) vB3)
  isoType Distrib0 = (Prod Zero vB, Zero)
  isoType Distrib = (Prod (Sum vB1 vB2) vB3, Sum (Prod vB1 vB3) (Prod vB2 vB3))

instance IsoType PIso where
  isoType PI1 = (One, One)
  isoType PZeroE = (Sum Zero vB, vB)
  isoType PSwapS = (Sum vB1 vB2, Sum vB2 vB1)
  isoType PAssocLS = (Sum vB1 (Sum vB2 vB3), Sum (Sum vB1 vB2) vB3)
  isoType PUnitE2 = (Prod One One, One)
  isoType PSwapP = (Prod vB1 vB2, Prod vB2 vB1)
  isoType PAssocLP = (Prod vB1 (Prod vB2 vB3), Prod (Prod vB1 vB2) vB3)
  isoType PDistrib0 = (Prod Zero vB, Zero)
  isoType PDistrib1 = ( Prod (Sum vB1 (Prod One vB2)) vB3
                      , Sum (Prod vB1 vB3) (Prod (Prod One vB2) vB3))

-------------------------------------------------------------------------------

freshVarId :: String -> TypeCheck Type
freshVarId prefix = do
  v <- tsVarId <$> get
  modify $ \s -> s { tsVarId = succ v }
  return $ TVar $ prefix ++ "." ++ show v

vB, vB1, vB2, vB3 :: Type
vB = TVar "b"
vB1 = TVar "a"
vB2 = TVar "b"
vB3 = TVar "c"

tr0 :: [Constraint] -> TypeResult
tr0 = flip TR mempty

freshVarABC :: String -> TypeCheck (Type, Type, Type)
freshVarABCD :: String -> TypeCheck (Type, Type, Type, Type)

freshVarABC prefix = do
  a <- freshVarId $ prefix ++ "$a"
  b <- freshVarId $ prefix ++ "$b"
  c <- freshVarId $ prefix ++ "$c"
  return (a, b, c)

freshVarABCD prefix = do
  (a, b, c) <- freshVarABC prefix
  d <- freshVarId $ prefix ++ "$d"
  return (a, b, c, d)

infer :: IsoType i => Cofree (FExprS i) () -> TypeCheck (Cofree (FExprS i) (Type, TypeResult))
infer (() :< EVar s) = do
  var <- freshVarId $ "$v$" ++ s
  return ((var, TR [] $ M.singleton s [var]) :< EVar s)

infer (() :< EIso iso) = do
  let (tin, tout) = isoType iso
  let (vin, vout) = (allVars tin, allVars tout)
  let vars = S.toList $ S.fromList (vin ++ vout)
  subs <- M.fromList . zip vars <$> mapM freshVarId vars
  let (tin', tout') = (subst subs tin, subst subs tout)
  t <- freshVarId "$iso"
  return ((t, tr0 [ t =:= tin' <-> tout' ]) :< EIso iso)

-- id :: a <-> a
infer (() :< EId) = do
  t <- freshVarId "$id"
  a <- freshVarId "$id$a"
  b <- freshVarId "$id$b"
  return ((t, tr0 [ t =:= a <-> a, a =:= b ]) :< EId)

-- f :: a <-> b
-- sym f :: b <-> a
infer (() :< ESym f) = do
  fe@((t', aTR) :< _) <- infer f
  t <- freshVarId "$sym"
  a <- freshVarId "$sym$a"
  b <- freshVarId "$sym$b"
  let p = (t, tr0 [ t =:= b <-> a, t' =:= a <-> b ] `mappend` aTR)
  return (p :< ESym fe)

-- f :: a <-> c, g :: c <-> b
-- f |> g :: a <-> b
infer (() :< ECompose f g) =
  inferResult2 f g "$co" ECompose $ \t (tf, tg) (a, b, c, d) ->
    [ t =:= a <-> b
    , tf =:= a <-> c
    , tg =:= c <-> b ]

-- f :: a <-> b, g :: c <-> d
-- f + g :: a + c <-> b + d
infer (() :< ESum f g) =
  inferResult2 f g "$sum" ESum $ \t (tf, tg) (a, b, c, d) ->
    [ t =:= Sum a c <-> Sum b d
    , tf =:= a <-> b
    , tg =:= c <-> d ]

-- f :: a <-> b, g :: c <-> d
-- f * g :: a * c <-> b * d
infer (() :< EProd f g) =
  inferResult2 f g "$prod" EProd $ \t (tf, tg) (a, b, c, d) ->
    [ t =:= Prod a c <-> Prod b d
    , tf =:= a <-> b
    , tg =:= c <-> d ]

inferResult2 f g prefix constr gC = do
  fe@((f, fTR) :< _) <- infer f
  ge@((g, gTR) :< _) <- infer g
  t <- freshVarId prefix
  (a, b, c, d) <- freshVarABCD prefix
  let p = (t, fTR `mappend` gTR `mappend` tr0 (gC t (f, g) (a, b, c, d)))
  return (p :< constr fe ge)

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

resolve :: TypeResult -> Type -> Either UnifyErr IType
resolve tr t = (\subs -> getTIso $ subst subs t) <$> sln where
  sln = solveConstraints . trConstraints $ tr

typeTree :: IsoType i => Cofree (FExprS i) () -> Either UnifyErr (Cofree (FExprS i) IType)
typeTree c = traverse (resolve tr0 . fst) cc where
  cc@((_, tr0) :< _) = evalState (infer c) initial
  initial = TS { tsVarId = 0 }

getTIso :: Type -> IType
getTIso (TIso a b) = ITIso a b
getTIso (TVar v) = ITVar v
getTIso t = error $ "getTIso: not an Iso type: " ++ show t

getType :: IsoType i => Cofree (FExprS i) () -> Either UnifyErr IType
getType = fmap (\(a :< _) -> a) . typeTree

