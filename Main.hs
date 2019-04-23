module Main where
import Data.List(isPrefixOf)
import Control.Exception
import Control.Monad.IO.Class
import System.Console.Repline
import qualified Data.Set as S

import Types
import Infer
import Parser
import FType
import GetIso
import Convert
import PrettyExpr

type Repl a = HaskelineT IO a

printEither (Left x) = putStrLn $ "*** " ++ show x
printEither (Right x) = putStrLn x

f1 g xs = liftIO ((printEither $ g $ unwords xs)
          `catch` (\e -> putStrLn $ "*** " ++ show (e :: ErrorCall)))

options :: [(String, [String] -> Repl ())]
options = [
    ("t", f1 typeFromExpr)
  , ("tp", f1 typeFromPExpr)
  , ("ce", f1 convExpr)
  , ("prop", f1 (r . prop_convExpr)) ]
  where r x = return (show x) :: Either () String

cmd :: String -> Repl ()
cmd input = liftIO $ print input

-- Tab Completion: return a completion for partial words entered
completer :: Monad m => WordCompleter m
completer n@(':':_) = return $ filter (isPrefixOf n) $ map ((":" ++) . fst) options
completer n = return $ S.toList $ S.filter (isPrefixOf n) words where
  words = S.fromList (map fst3 isos ++ map snd3 isos)
  fst3 (a, _, _) = a
  snd3 (_, b, _) = b

testType = () :< ECompose (() :< ESym (() :< EIso ZeroE))
                          (() :< EIso SwapS)

uniteS :: Either UnifyErr (Cofree NFExpr IType)
uniteS = typeTree $ parseExpr "unite; uniti; swapP; distrib; (swapP + swapP); (unite + unite)"

joinEither :: Either a (Either b c) -> Either (Either a b) c
joinEither (Left x) = Left (Left x)
joinEither (Right (Left x)) = Left (Right x)
joinEither (Right (Right x)) = Right x

typeFromExpr s = (\(t :< _) -> prettyIType t) <$> typeTree (parseExpr s)
typeFromPExpr s = (\(t :< _) -> prettyIType t) <$> typeTree (parsePExpr s)

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
main = evalRepl (pure ">>> ") cmd options (Just ':') (Word completer) ini

