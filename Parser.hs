module Parser (ParseError, Parser, typeParser, parseType, parseExpr) where
import Data.Maybe
import qualified Data.Map as M
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Expr
import Text.Parsec.Language(haskellStyle)

import Types

type Parser = ParsecT String () Identity

langDef = haskellStyle
lang = P.makeTokenParser langDef

ws = P.whiteSpace lang
ident = P.identifier lang
natural = P.natural lang
parens = P.parens lang
reserved = P.reserved lang
reservedOp = P.reservedOp lang

typeTable = [ [binary "*" Prod AssocLeft, hiddenBinary Prod AssocLeft ]
            , [binary "+" Sum AssocLeft ]
            , [binary "<->" TIso AssocLeft ] ]

typeExpr, typeParser :: Parser Type
typeExpr = buildExpressionParser typeTable typeTerm <?> "typeExpr"

typeParser = typeExpr

typeTerm :: Parser Type
typeTerm = parens typeExpr <|> typeVar <|> typeNum <?> "typeTerm"

typeVar = TVar <$> ident
typeNum = fromN <$> natural where
  fromN 0 = Zero
  fromN 1 = One
  fromN k = Sum One (fromN (k - 1))

exprTable = [ [prefixReserved "sym" (u ESym) ]
            , [binary ";" (b ECompose) AssocLeft ]
            , [binary "*" (b EProd) AssocLeft, hiddenBinary (b EProd) AssocLeft ]
            , [binary "+" (b ESum) AssocLeft ] ] where
  u f a = () :< f a
  b f a b = () :< f a b

expr, exprParser :: Parser Expr
expr = buildExpressionParser exprTable exprTerm <?> "expr"

exprParser = expr

exprTerm :: Parser Expr
exprTerm = parens expr <|> exprIdent <?> "exprTerm"

exprIdent = getFromIdent <$> ident where
  getFromIdent v = fromMaybe (() :< EVar v) $ M.lookup v idents

  idents :: Map String Expr
  idents = M.fromList [ ("id", () :< EId)
                      , ("swapP", iso SwapP)
                      , ("swapS", iso SwapS) ]
           `M.union` M.fromList (concat [ [(a, iso i), (b, iso' i)]
                                        | (a, b, i) <- isos])

  isos = [ ("zeroe", "zeroi", ZeroE)
         , ("assocLS", "assocRS", AssocLS)
         , ("unite", "uniti", Unite)
         , ("assocLP", "assocRP", AssocLP)
         , ("distrib0", "factor0", Distrib0)
         , ("distrib", "factor", Distrib) ]

  iso x = () :< EIso x
  iso' x = () :< ESym (() :< EIso x)

hiddenBinary fun = Infix (ws >> return fun)
binary name fun = Infix (reservedOp name >> return fun)
prefixReserved name fun = Prefix (reserved name >> return fun)
prefix name fun = Prefix (reservedOp name >> return fun)
postfix name fun = Postfix (reservedOp name >> return fun)

parseType :: String -> Type
parseType = either (error . show) id . parse (typeParser <* eof) ""

parseExpr :: String -> Expr
parseExpr = either (error . show) id . parse (exprParser <* eof) ""

