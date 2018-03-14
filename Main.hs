module Main where
import Types
import Infer
import Parser
import FType
import Convert

testType = () :< ECompose (() :< ESym (() :< EIso ZeroE))
                          (() :< EIso SwapS)

uniteP :: Either UnifyErr (Cofree NFExpr Type)
uniteP = typeTree $ parseExpr "unite; uniti; swapP; distrib; (swapP + swapP); (unite + unite)"

main :: IO ()
main = print $ typeTree testType

