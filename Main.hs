module Main where
import Data.List(isPrefixOf)
import Control.Exception
import Control.Monad.IO.Class
import System.Console.Repline

import Types
import Infer
import Parser
import FType
import Convert
import PrettyExpr

type Repl a = HaskelineT IO a

f1 g xs = liftIO ((print $ g $ unwords xs) `catch` (\e -> print (e :: ErrorCall)))

options :: [(String, [String] -> Repl ())]
options = [
    ("t", f1 typeFromExpr)
  , ("tp", f1 typeFromPExpr)
  , ("ce", f1 convExpr)
  , ("prop", f1 prop_convExpr)
  ]

cmd :: String -> Repl ()
cmd input = liftIO $ print input

-- Tab Completion: return a completion for partial words entered
completer :: Monad m => WordCompleter m
completer n = return $ filter (isPrefixOf n) $ map fst options

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

prop_convExpr e = t `seq` t' `seq` t == t' where
  t = toM $ typeFromExpr e
  t' = toM $ joinEither $ typeFromPExpr <$> convExpr e
  toM = either (const Nothing) Just

ini :: Repl ()
ini = liftIO $ return ()

main :: IO ()
main = evalRepl ">>> " cmd options (Word completer) ini

