module Parser (parse, parseShow, parseExamples) where

import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec hiding (parse)
import Lib (Sym, Expr (..), Levels, showLam)
import Data.Functor
import Data.Map.Strict (fromList)
import qualified Text.Parsec as P

sym :: Parser Sym
sym = do
  l <- letter
  ls <- many alphaNum
  return $ l:ls

levelT :: Parser Expr
levelT = char 'L' $> L

lv :: Parser (Sym, Int)
lv = do
  s <- sym <* char '+'
  i <- read <$> many1 digit
  return (s, i)

level :: Parser Expr
level = uncurry (:+) <$> lv

levels :: Parser Levels
levels = fromList <$> sepBy lv (char ',')

universe :: Parser Expr
universe = U <$> (char 'U' *> space *> levels)

lambda :: Parser Expr
lambda = do
  s <- char '(' *> sym
  t <- char ':' *> space *> expr
  e <- char ')' *> space *> string "->" *> space *> expr
  return $ (s, t) :-> e

application :: Parser Expr
application = do
  f <- exprR <* space
  leftAssociate f <$> expr

leftAssociate :: Expr -> Expr -> Expr
leftAssociate f (x :@ xs) = leftAssociate (f :@ x) xs
leftAssociate f x = f :@ x

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
expr = try parens <|> levelT <|> universe <|> lambda <|> try typing <|> try application <|> try level <|> symbol

exprT :: Parser Expr
exprT = try parens <|> levelT <|> universe <|> lambda <|> try application <|> try level <|> symbol

exprR :: Parser Expr
exprR = try parens <|> levelT <|> universe <|> lambda <|> try level <|> symbol

parse :: String -> Either ParseError Expr
parse = P.parse (expr <* eof) ""

parseShow :: Sym -> String -> IO ()
parseShow s e = case parse e of
  Right e' -> showLam s e'
  Left err -> putStrLn $ s ++ ": " ++ show err

parseExamples :: IO ()
parseExamples = do
  parseShow "l" "(i: L) -> i+0"
  parseShow "id" "(i: L) -> (T: U i+0) -> (tt: (t1: T) -> (t2: T) -> T) -> (t: T) -> T :> tt t t"
  --             |         |         |         |         |         |         |         |         |         |
  --             0         10        20        30        40        50        60        70        80        90
