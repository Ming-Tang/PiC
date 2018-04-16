module Parser (

-- * Types
  ParseError
, TypeParser
, ExprParser

-- * Parsers
, typeParser
, exprParser

-- * Parse
, parseType
, parseExpr
, parsePExpr

) where
import Data.Maybe
import qualified Data.Map as M
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Expr
import Text.Parsec.Language(haskellStyle)

import Types
import GetIso

type TypeParser = ParsecT String () Identity
type ExprParser = ParsecT String () Identity

langDef = haskellStyle
lang = P.makeTokenParser langDef

ws = P.whiteSpace lang
ident = P.identifier lang
natural = P.natural lang
parens = P.parens lang
reserved = P.reserved lang
reservedOp = P.reservedOp lang

typeTable = [ [ binary "*" Prod AssocLeft
              , hiddenBinary Prod AssocLeft
                -- Haskell counterpart compatibility
              , binary "&" Prod AssocLeft ]
            , [ binary "+" Sum AssocLeft ]
            , [ binary "<->" TIso AssocLeft ] ]

typeExpr, typeParser :: TypeParser Type
typeExpr = buildExpressionParser typeTable typeTerm <?> "typeExpr"

typeParser = typeExpr

typeTerm :: TypeParser Type
typeTerm = parens typeExpr <|> typeVar <|> typeNum <?> "typeTerm"

typeVar = f <$> ident where
  f "U" = One
  f "Z" = Zero
  f x = TVar x

typeNum = fromN <$> natural where
  fromN 0 = Zero
  fromN 1 = One
  fromN k = Sum (fromN (k - 1)) One

exprTable :: GetIso i => [[Operator String u Identity (IExpr i)]]
exprTable = [ [ prefixReserved "sym" (u ESym) ]         -- 4
            , [ binary ";" (b ECompose) AssocLeft       -- 3
                -- Haskell counterpart compatibility
              , binary "|>" (b ECompose) AssocLeft ]
            , [ binary "*" (b EProd) AssocLeft          -- 2
              , hiddenBinary (b EProd) AssocLeft ]
            , [ binary "+" (b ESum) AssocLeft ] ] where -- 1
  u f a = () :< f a
  b f a b = () :< f a b

expr, exprParser, exprTerm, exprIdent :: GetIso i => ExprParser (IExpr i)
expr = buildExpressionParser exprTable exprTerm <?> "expr"

exprParser = expr

exprTerm = parens expr <|> exprIdent <?> "exprTerm"

exprIdent = getFromIdent <$> ident where
  getFromIdent v = fromMaybe (() :< EVar v) $ M.lookup v idents

  idents :: GetIso i => Map String (IExpr i)
  idents = M.fromList [ ("id", () :< EId) ]
           `M.union` M.fromList (
             concat [ if a == b then [(a, iso i)]
                      else [(a, iso i), (b, iso' i)]
                    | (a, b, i) <- isoNames ])

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

parsePExpr :: String -> PExpr
parsePExpr = either (error . show) id . parse (exprParser <* eof) ""

