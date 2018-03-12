module Main where
import Types
import Infer
import Parser
import FType

testType = () :< ECompose (() :< ESym (() :< EIso ZeroE))
                          (() :< EIso SwapS)

main :: IO ()
main = print $ typeTree testType

