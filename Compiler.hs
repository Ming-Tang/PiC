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

type Perm = [Integer]

data PrimOp = Mov Perm   -- #n <-> #n
            | CMov       -- #2 <-> #3
            | Dup        -- #1 <-> #2
  deriving (Eq, Ord, Show)

data CNode t a = CGroup String a
               | CComp (CList t a)
               | CSum (CTree t a)
               | CProd (CTree t a)
               | CPrim PrimOp
  deriving (Eq, Ord, Show)

-- | A compiled reversible program with type annotation 't' at any point.
type Compiled t = Cofree (CNode t) (IsoT t)

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
