module Main where
import Types
import Infer
import Parser
import FType
import Convert
import PrettyExpr

testType = () :< ECompose (() :< ESym (() :< EIso ZeroE))
                          (() :< EIso SwapS)

uniteS :: Either UnifyErr (Cofree NFExpr IType)
uniteS = typeTree $ parseExpr "unite; uniti; swapP; distrib; (swapP + swapP); (unite + unite)"

joinEither :: Either a (Either b c) -> Either (Either a b) c
joinEither (Left x) = Left (Left x)
joinEither (Right (Left x)) = Left (Right x)
joinEither (Right (Right x)) = Right x

typeFromExpr s = (\(t :< _) -> t) <$> typeTree (parseExpr s)
typeFromPExpr s = (\(t :< _) -> t) <$> typeTree (parsePExpr s)

typeTreeExpr s = typeTree $ parseExpr s
typeTreePExpr s = typeTree $ parsePExpr s

convExpr s = joinEither $ (prettyExpr <$>) . convertExpr <$> typeTree (parseExpr s)

main :: IO ()
main = print $ typeTree testType

