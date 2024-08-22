{-# LANGUAGE TupleSections #-}
module Parser (parse, parseShow, parseExamples) where

import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec hiding (parse)
import Lib (Sym, Expr (..), Levels, showLam, Inter (..))
import Data.Functor
import Data.Map.Strict (fromList)
import qualified Text.Parsec as P
import Data.Foldable (foldl')

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
  v <- opExpr <* char ';' <* spaces <* comments
  (E s v <$> opExpr) <|> return v

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

introduce :: Parser Expr
introduce = do
  s <- char '(' *> sym
  t <- char ':' *> opExpr
  let
    lambda = do
      e <- char ')' *> spaces *> string "->" *> opExpr
      return $ (s, t) :-> e
    intersection = do
      t2 <- string "/\\" *> opExpr <* char ')'
      return $ (s, t) :/\ t2
  lambda <|> intersection

symbol :: Parser Expr
symbol = S <$> sym

erased :: Parser Expr
erased = char '\'' *> (Erased <$> expr)

parens :: Parser Expr
parens = char '[' *> opExpr <* char ']'

comment :: Parser String
comment = end <|> inner
  where
    end = mappend <$> try (string "--") <*> many (noneOf ['\n'])
    inner = mappend <$> try (string "{- ") <*> ((mappend <$> inner <*> innerRight) <|> innerRight)
    innerRight = try (string " -}") <|> ((:) <$> anyToken <*> innerRight)

comments :: Parser [String]
comments = many (comment <* spaces)

surroundCommentSpaces :: Parser Expr -> Parser Expr
surroundCommentSpaces p = spaces *> comments *> p <* spaces <* comments

baseExpr :: Parser Expr
baseExpr = erased <|> named <|> special <|> introduce <|> symbol <|> parens

expr :: Parser Expr
expr = foldl' (.) surroundCommentSpaces posts baseExpr
  where
    postInter = post ".1" (`I` One) . post ".2" (`I` Two)
    posts = [postInter]

opExpr :: Parser Expr
opExpr = foldl' (.) id ops expr
    where
      opApp = associate ALeft "" (:@)
      opIntersect = associate ARight "^" (:^)
      opType = associate ANone ":>" (::>)
      ops = [opType, opIntersect, opApp]

data Associate = ALeft | ARight | ANone

post :: String -> (Expr -> Expr) -> Parser Expr -> Parser Expr
post s op p = do
  e <- p
  (try (string s) $> op e) <|> return e

associate :: Associate -> String -> (Expr -> Expr -> Expr) -> Parser Expr -> Parser Expr
associate a n op p = do
  l <- p
  r <- many (string n *> p)
  go a l r
  where
    go _ f [] = return f
    go _ f [x] = return  $ f `op` x
    go ANone _f _xs = unexpected $ "operator \"" ++ n ++ "\" is not associative."
    go ALeft f (x:xs) = go ALeft (f `op` x) xs
    go ARight f (x:xs) = op f <$> go ARight x xs

parse :: String -> Either ParseError Expr
parse = P.parse (opExpr <* eof) ""

parseShow :: Sym -> String -> IO ()
parseShow s e = case parse e of
  Right e' -> showLam s e'
  Left err -> putStrLn $ s ++ ": " ++ show err

parseExamples :: IO ()
parseExamples = do
  parseShow "zero" "(i: #L) -> (P: #U i) -> (s: (p: P) -> P) -> (z: P) -> z"
  parseShow "id" "(i: '#L) -> @Z = #U i; (T: 'Z) -> (tt: (t1: T) -> (t2: T) -> T) -> (t: T) -> @r = T; r :> @ttt = tt t; ttt t"

  parseShow " l" "(i: #L) -> #i+0"
  parseShow " U" "#U +1"
  parseShow "id" "(i: #L) -> (T: #U i) -> (x: T) -> T :> x"
  --             |         |         |         |         |         |         |         |         |         |         |         |
  --             0         10        20        30        40        50        60        70        80        90        100       110
