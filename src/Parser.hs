module Parser (parse) where

import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec hiding (parse)
import Lib hiding (level)
import Data.Functor
import qualified Text.Parsec as P

levelT :: Parser Expr
levelT = char 'L' $> L

level :: Parser Expr
level = do
  s <- sym
  i <- read <$> many1 digit
  return $ s :+ i

sym :: Parser Sym
sym = many letter

symbol :: Parser Expr
symbol = S <$> sym

lambda :: Parser Expr
lambda = do
  s <- char '(' *> sym
  t <- char ':' *> spaces *> expr
  e <- char ')' *> spaces *> string "->" *> spaces *> expr
  return $ (s, t) :-> e

expr :: Parser Expr
expr = lambda <|> symbol <|> levelT <|> level

parse :: String -> Either ParseError Expr
parse = P.parse expr ""
