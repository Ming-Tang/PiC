{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Compiler where
import Control.Monad.Except

import Types
import Infer

newtype Perm = Perm { getPerm :: [Integer] }
  deriving (Eq, Ord, Show)

data PrimOp = Mov Perm      -- n <-> n
            | CMov Integer  -- 2 + n <-> 1 + 2 n
            | Dup           -- 1 <-> 2
  deriving (Eq, Ord, Show)

data CNode a = CGroup String a
             | CPrim PrimOp
             | CCompose a a
             | CSum a a
             | CProd a a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Size = Size { sizeInOut :: (Integer, Integer)
                 , sizeMinMax :: (Integer, Integer) }
  deriving (Eq, Ord, Show)

data CompileError = TVarNotSupported String
                  | EVarNotSupported String
                  | SizeMismatch (Integer, Integer) (Type, Type)
  deriving (Eq, Ord, Show)

type Compile = Either CompileError
type CompileResult = Either CompileError (Cofree CNode ((Type, Type), Size))

typeSize :: Type -> Either CompileError Integer
typeSize (TVar v) = throwError $ TVarNotSupported v
typeSize Zero = return 0
typeSize One = return 1
typeSize (Sum a b) = (+) <$> typeSize a <*> typeSize b
typeSize (Prod a b) = (*) <$> typeSize a <*> typeSize b

composeSize, parallelSize :: Size -> Size -> Size
composeSize sf sg = Size (p, q) (min sa sb, max sc sd) where
  (Size (p, _) (sa, sc)) = sf
  (Size (_, q) (sb, sd)) = sg

parallelSize sf sg = Size (pa + pb, qa + qb) (min sa sb, max sc sd) where
  (Size (pa, qa) (sa, sc)) = sf
  (Size (pb, qb) (sb, sd)) = sg

compile :: Cofree PFExpr IType -> CompileResult
compile (ITVar v :< _) = throwError $ TVarNotSupported v
compile (_ :< EVar v) = throwError $ EVarNotSupported v

compile (ITIso a b :< EIso PI1) = idNode a b
compile (ITIso a b :< EIso PZeroE) = idNode a b
compile (ITIso a b :< EIso PSwapS) = swapNode a b
compile (ITIso a b :< EIso PAssocLS) = idNode a b
compile (ITIso a b :< EIso PUnitE2) = dupNode a b
compile (ITIso a b :< EIso PSwapP) = swapNode a b
compile (ITIso a b :< EIso PAssocLP) = idNode a b
compile (ITIso a b :< EIso PDistrib0) = idNode a b
compile (ITIso a b :< EIso PDistrib1) = distrib1Node a b

compile (ITIso a b :< ESym n) = do
  n'@(r :< _) <- symNode <$> compile n
  return (r :< CGroup "sym" n')

compile (ITIso a b :< ECompose f g) = do
  f'@((_, sf) :< _) <- compile f
  g'@((_, sg) :< _) <- compile g
  let s = composeSize sf sg
  return (((a, b), s) :< CCompose f' g')

compile (ITIso a b :< ESum f g) = compile2 CSum a b f g

compile2 c a b f g = do
  f'@((_, sf) :< _) <- compile f
  g'@((_, sg) :< _) <- compile g
  let s = parallelSize sf sg
  return (((a, b), s) :< c f' g')

symNode :: Cofree CNode ((Type, Type), Size) -> Cofree CNode ((Type, Type), Size)
symNode _ = error "symNode"

idPrim n = (Size (n, n) (1, 1), CPrim (Mov (Perm [0..n-1])))
dupPrim n = (Size (n, n) (1, 2), CPrim Dup)

constSize :: Type -> Type -> (Integer -> Compile a) -> Compile a
constSize a b f = do
  ts <- typeSize a
  ts' <- typeSize b
  if ts /= ts' then
    throwError $ SizeMismatch (ts, ts') (a, b)
  else
    f ts

idNode, swapNode, dupNode, distrib1Node :: Type -> Type -> CompileResult
idNode a b = constSize a b $ \ts ->
  let (s, n) = idPrim ts in
  return (((a, b), s) :< n)

dupNode a b = error "dupNode"

swapNode a b = error "swapNode"

distrib1Node a b = error "distrib1Node"

primOpSize :: PrimOp -> Size
primOpSize (Mov p) = Size (n, n) (1, 1) where n = fromIntegral $ length $ getPerm p
primOpSize (CMov n) = Size (n + 2, 1 + 2 * n) (1, 1)
primOpSize Dup = Size (1, 2) (1, 1)

cexprSize :: Cofree CNode a -> Size
cexprSize (_ :< CGroup _ ce) = cexprSize ce
cexprSize (_ :< CPrim p) = primOpSize p
cexprSize (_ :< CCompose a b) = Size (p, q) (min sa sb, max sc sd) where
    (Size (p, _) (sa, sc)) = cexprSize a
    (Size (_, q) (sb, sd)) = cexprSize b

cexprSize (_ :< CSum a b) = cexprSize2 a b
cexprSize (_ :< CProd a b) = cexprSize2 a b

cexprSize2 a b = Size (pa + pb, qa + qb) (min sa sb, max sc sd) where
    (Size (pa, qa) (sa, sc)) = cexprSize a
    (Size (pb, qb) (sb, sd)) = cexprSize b

