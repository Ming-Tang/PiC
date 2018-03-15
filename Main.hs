module Main where
import Types
import Infer
import Parser
import FType
import Convert

testType = () :< ECompose (() :< ESym (() :< EIso ZeroE))
                          (() :< EIso SwapS)

uniteS :: Either UnifyErr (Cofree NFExpr IType)
uniteS = typeTree $ parseExpr "unite; uniti; swapP; distrib; (swapP + swapP); (unite + unite)"

main :: IO ()
main = print $ typeTree testType

