{-# LANGUAGE TupleSections #-}
module Parser (parse, parseShow, parseExamples) where

import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec hiding (parse)
import Lib (Sym, Expr (..), Levels, showLam)
import Data.Functor
import Data.Map.Strict (fromList)
import qualified Text.Parsec as P

validSym :: Parser Char
validSym = alphaNum

sym :: Parser Sym
sym = do
  l <- letter
  ls <- many validSym
  return $ l:ls

named :: Parser Expr
named = do
  s <- sym <* space <* char '=' <* space
  v <- expr <* string "; "
  E s v <$> expr


levelT :: Parser Expr
levelT = char 'L' $> L

lv :: Parser (Sym, Int)
lv = do
  s <- many validSym <* char '+'
  i <- read <$> many1 digit
  return (s, i)

level :: Parser Expr
level = uncurry (:+) <$> lv

levels :: Parser Levels
levels = fromList <$> sepBy (try lv <|> (,0) <$> sym) (char ',')

universe :: Parser Expr
universe = U <$> (char 'U' *> space *> levels)

erasure :: Parser Bool
erasure = char '\'' $> True <|> return False

lambda :: Parser Expr
lambda = do
  er <- char '(' *> erasure
  s <- sym <* char ':' <* space
  t <- expr <* char ')' 
  e <- space *> string "->" *> space *> expr
  return $ Lam er s t e

application :: Parser Expr
application = do
  f <- exprR <* space
  er <- erasure
  leftAssociate er f <$> expr

leftAssociate :: Bool -> Expr -> Expr -> Expr
leftAssociate er f (App er' x xs) = leftAssociate er' (App er f x) xs
leftAssociate er f x = App er f x

typing :: Parser Expr
typing = do
  t <- exprT <* space <* string ":>" <* space
  e <- exprT
  return $ t ::> e

symbol :: Parser Expr
symbol = S <$> sym

parens :: Parser Expr
parens = char '(' *> expr <* char ')'

expr :: Parser Expr
expr = try named <|> try parens <|> levelT <|> universe <|> lambda <|> try typing <|> try application <|> try level <|> symbol

exprT :: Parser Expr
exprT = try named <|> try parens <|> levelT <|> universe <|> lambda <|> try application <|> try level <|> symbol

exprR :: Parser Expr
exprR = try named <|> try parens <|> levelT <|> universe <|> lambda <|> try level <|> symbol

parse :: String -> Either ParseError Expr
parse = P.parse (expr <* eof) ""

parseShow :: Sym -> String -> IO ()
parseShow s e = case parse e of
  Right e' -> showLam s e'
  Left err -> putStrLn $ s ++ ": " ++ show err

parseExamples :: IO ()
parseExamples = do
  parseShow " l" "('i: L) -> i+0"
  parseShow "id" "('i: L) -> Z = U i; ('T: Z) -> (tt: (t1: T) -> (t2: T) -> T) -> (t: T) -> r = T; r :> ttt = tt t; ttt t"
  parseShow " U" "U +1"
  --             |         |         |         |         |         |         |         |         |         |
  --             0         10        20        30        40        50        60        70        80        90
