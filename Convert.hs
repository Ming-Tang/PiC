module Convert (convertExpr, convertIso) where
import Types
import Infer

data ConvertErr = EVarErr String
                | ETypeErr (Type, Type) Iso
 deriving (Eq, Ord, Show)

convertExpr :: Cofree NFExpr Type -> Either ConvertErr PExpr
convertExpr (TIso a b :< EIso i) = convertIso (a, b) i
convertExpr (_ :< EId) = return (() :< EId)
convertExpr (_ :< ESym a) = ((() :<) . ESym) <$> convertExpr a
convertExpr (_ :< ECompose a b) = convertExpr2 ECompose a b
convertExpr (_ :< ESum a b) = convertExpr2 ESum a b
convertExpr (_ :< EProd a b) = convertExpr2 EProd a b
convertExpr (_ :< EVar v) = Left (EVarErr v)

convertExpr2 f a b = fmap (() :<) (f <$> convertExpr a <*> convertExpr b)

convertIso :: (Type, Type) -> Iso -> Either ConvertErr PExpr
convertIso _ ZeroE = return $ iso PZeroE
convertIso _ SwapS = return $ iso PSwapS
convertIso _ AssocLS = return $ iso PAssocLS
convertIso ts@(Prod One b', b) UnitE | b == b' = return $ convertUnitE b
convertIso _ SwapP = return $ iso PSwapP
convertIso _ AssocLP = return $ iso PAssocLP
convertIso _ Distrib0 = return $ iso PDistrib0
convertIso (Prod (Sum a b) c, Sum (Prod a' c') (Prod b' c'')) Distrib
  | a == a' && b == b' && c == c' && c' == c'' = return $ convertDistrib (a, b) c
convertIso a b = Left (ETypeErr a b)

infixl 4 |>
infixl 5 .+.
infixl 6 .*.

a |> b = () :< ECompose a b
a .*. b = () :< EProd a b
a .+. b = () :< EProd a b

iso x = () :< EIso x
eid = () :< EId
swapP = () :< EIso PSwapP
distrib1 = () :< EIso PDistrib1

convertUnitE :: Type -> PExpr
convertUnitE Zero = swapP |> iso PDistrib0
convertUnitE One = iso PUnitE2
convertUnitE (Sum a b)
    = eid .*. (eid .+. unitiB) |> swapP |> distrib1
      |> ((swapP |> uniteA) .+. (swapP |> unitePB |> uniteB)) where
  uniteA = convertUnitE a
  uniteB = convertUnitE b
  unitePB = convertUnitE (Prod One b)
  unitiB = () :< ESym uniteB

convertUnitE (Prod a b) = iso PAssocLP |> unite .*. eid where
  unite = convertUnitE a

convertDistrib :: (Type, Type) -> Type -> PExpr
convertDistrib (_, Prod One b) _ = distrib1
convertDistrib (a, b) c
    = (eid .+. unitiB) .*. eid |> distrib1 |> (eid .+. (uniteB .*. eid)) where
    uniteB = convertUnitE b
    unitiB = () :< ESym uniteB

