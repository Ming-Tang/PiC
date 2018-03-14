{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Compiler where
import Types
import Infer

infixr 5 :->
infixr 4 :+:

type IsoT t = (t, t)

data CChain f a = f :-> a | CNil
  deriving (Eq, Ord, Show)

data CBranch f a = a :+: a | BLeaf f
  deriving (Eq, Ord, Show)

type CList t a = Cofree (CChain a) t
type CTree t a = Cofree (CBranch a) (IsoT t)

newtype Perm = Perm { getPerm :: [Integer] }
  deriving (Eq, Ord, Show)

data PrimOp = Mov Perm      -- n <-> n
            | CMov Integer  -- 2 + n <-> 1 + 2 n
            | Dup           -- 1 <-> 2
  deriving (Eq, Ord, Show)

data CNode a = CGroup String a
             | CPrim PrimOp
             | CComp a a
             | CSum a a
             | CProd a a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | A compiled reversible program with type annotation 't' at any point.
type Compiled t = Cofree CNode (IsoT t)

type Size = (Integer, Integer)

primOpSize :: PrimOp -> Size
primOpSize (Mov p) = (n, n) where n = fromIntegral $ length $ getPerm p
primOpSize (CMov n) = (n + 2, 1 + 2 * n)
primOpSize Dup = (1, 2)

cexprSize :: Compiled a -> Size
cexprSize (_ :< CGroup _ ce) = cexprSize ce
cexprSize (_ :< CPrim p) = primOpSize p
cexprSize (_ :< CComp a b) = (p, q) where (p, _) = cexprSize a
                                          (_, q) = cexprSize b

cexprSize (_ :< CSum a b) = cexprSize2 a b
cexprSize (_ :< CProd a b) = cexprSize2 a b

cexprSize2 a b = (pa + pb, qa + qb) where (pa, qa) = cexprSize a
                                          (pb, qb) = cexprSize b


testCC :: Cofree (CChain String) String
testCC = "b * bool" :< "swapP" :-> "bool * b" :< "distrib"
                    :-> "1*b + 1*b" :< "unite + unite" :-> "b + b" :< CNil

testCB0, testCB1 :: Cofree (CBranch String) (String, String)
testCB0 = ("a + b", "x + y") :< (("a", "x") :< BLeaf "id"
                                 :+:
                                 ("b", "y") :< BLeaf "id")

testCB1 = ("(a+b) + (c+(d+e))", "(x+y) + (z+(v+w))")
            :< (("a+b", "x+y") :< (("a", "x") :< BLeaf "id"
                                   :+:
                                   ("b", "y") :< BLeaf "id")
                :+:
                ("c+(d+e)", "z+(v+w)") :< (("c", "z") :< BLeaf "id"
                                            :+:
                                            ("d+e", "v+w")
                                                :< (("d", "v") :< BLeaf "id"
                                                    :+:
                                                    ("e", "w") :< BLeaf "id")))

