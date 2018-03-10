module Main where

import Types
import Infer

testType = () :< ECompose (() :< ESym (() :< EIso ZeroE))
                          (() :< EIso SwapS)

main :: IO ()
main = print $ typeTree testType

