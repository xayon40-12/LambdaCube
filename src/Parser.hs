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
  s <- char '@' *> sym <* spaces <* char '='
  v <- expr <* string ";"
  E s v <$> expr

special :: Parser Expr
special = char '#' *> (levelT <|> universe <|> level)

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
levels = fromList <$> sepBy1 (try lv <|> ((,0) <$> sym)) (char ',')

universe :: Parser Expr
universe = U <$> (char 'U' *> space *> levels)

lambda :: Parser Expr
lambda = do
  s <- char '(' *> sym
  t <- char ':' *> expr
  e <- char ')' *> spaces *> string "->" *> expr
  return $ (s, t) :-> e

application :: Parser Expr
application = do
  f <- exprR
  xs <- many1 exprR
  return $ app f xs
  where
    app f [] = f
    app f (x:xs) = app (f :@ x) xs

typing :: Parser Expr
typing = do
  t <- exprT <* string ":>"
  e <- exprT
  return $ t ::> e

symbol :: Parser Expr
symbol = S <$> sym

erased :: Parser Expr
erased = char '\'' *> (Erased <$> expr)

parens :: Parser Expr
parens = char '[' *> expr <* char ']'

comment :: Parser Expr
comment = (string "--" *> many (noneOf ['\n']) *> many (char '\n') *> expr) <|> (string "{- " *> right *> expr)
  where right = try (string " -}") <|> (anyToken *> right)

exprConstr :: Bool -> Bool -> Parser Expr
exprConstr enableTyping enableApplication = spaces *> p1 <* spaces
  where
    p1 = comment <|> p2 enableTyping
    p2 True = try typing <|> p3
    p2 False = p3
    p3 = erased <|> named <|> special <|> lambda <|> p4 enableApplication
    p4 True = try application <|> p5
    p4 False = p5
    p5 = symbol <|> parens

expr :: Parser Expr
expr = exprConstr True True

exprT :: Parser Expr
exprT = exprConstr False True

exprR :: Parser Expr
exprR = exprConstr False False

parse :: String -> Either ParseError Expr
parse = P.parse (expr <* eof) ""

parseShow :: Sym -> String -> IO ()
parseShow s e = case parse e of
  Right e' -> showLam s e'
  Left err -> putStrLn $ s ++ ": " ++ show err

parseExamples :: IO ()
parseExamples = do
  parseShow " l" "(i: L) -> i+0"
  parseShow "id" "(i: 'L) -> Z = U i; (T: 'Z) -> (tt: (t1: T) -> (t2: T) -> T) -> (t: T) -> r = T; r :> ttt = tt t; ttt t"
  parseShow " U" "U +1"
  --             |         |         |         |         |         |         |         |         |         |
  --             0         10        20        30        40        50        60        70        80        90
