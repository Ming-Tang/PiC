module Convert (Convert, ConvertErr(..), convertExpr, convertIso) where
import Types
import Infer
import Control.Monad.Except(throwError)

infixl 4 |>
infixl 5 .+.
infixl 6 .*.

a |> b = () :< ECompose a b
a .*. b = () :< EProd a b
a .+. b = () :< EProd a b

data ConvertErr = EIsoTypeErr (Type, Type) Iso
                | UnitENotSupported Type
                | ExprNotSupported (Cofree NFExpr IType)
                | TypeVarNotSupported String
 deriving (Eq, Ord, Show)

type Convert = Either ConvertErr

convertExpr :: Cofree NFExpr IType -> Convert PExpr
convertExpr (ITIso a b :< EIso i) = convertIso (a, b) i
convertExpr (ITVar v :< _) = throwError $ TypeVarNotSupported v
convertExpr (_ :< EId) = return (() :< EId)
convertExpr (_ :< ESym a) = ((() :<) . ESym) <$> convertExpr a
convertExpr (_ :< ECompose a b) = convertExpr2 ECompose a b
convertExpr (_ :< ESum a b) = convertExpr2 ESum a b
convertExpr (_ :< EProd a b) = convertExpr2 EProd a b
convertExpr e = throwError $ ExprNotSupported e

convertExpr2 f a b = fmap (() :<) (f <$> convertExpr a <*> convertExpr b)

convertIso :: (Type, Type) -> Iso -> Convert PExpr
convertIso _ ZeroE = return $ iso PZeroE
convertIso _ SwapS = return $ iso PSwapS
convertIso _ AssocLS = return $ iso PAssocLS
convertIso ts@(Prod One b', b) UnitE | b == b' = convertUnitE b
convertIso _ SwapP = return $ iso PSwapP
convertIso _ AssocLP = return $ iso PAssocLP
convertIso _ Distrib0 = return $ iso PDistrib0
convertIso (Prod (Sum a b) c, Sum (Prod a' c') (Prod b' c'')) Distrib
  | a == a' && b == b' && c == c' && c' == c'' = convertDistrib (a, b) c

convertIso a b = throwError (EIsoTypeErr a b)

iso x = () :< EIso x
eid = () :< EId
swapP = () :< EIso PSwapP
distrib1 = () :< EIso PDistrib1

convertUnitE :: Type -> Convert PExpr
convertUnitE Zero = return (swapP |> iso PDistrib0)
convertUnitE One = return $ iso PUnitE2
convertUnitE (Sum a b) = do
  uniteA <- convertUnitE a
  uniteB <- convertUnitE b
  unitePB <- convertUnitE (Prod One b)
  let unitiB = () :< ESym uniteB
  let res = eid .*. (eid .+. unitiB) |> swapP |> distrib1
           |> ((swapP |> uniteA) .+. (swapP |> unitePB |> uniteB))
  return res

convertUnitE (Prod a b) = do
  unite <- convertUnitE a
  let res = iso PAssocLP |> unite .*. eid
  return res

convertUnitE t = throwError (UnitENotSupported t)

convertDistrib :: (Type, Type) -> Type -> Convert PExpr
convertDistrib (_, Prod One b) _ = return distrib1
convertDistrib (a, b) c = do
  uniteB <- convertUnitE b
  let unitiB = () :< ESym uniteB
  let res = (eid .+. unitiB) .*. eid |> distrib1 |> (eid .+. (uniteB .*. eid))
  return res

