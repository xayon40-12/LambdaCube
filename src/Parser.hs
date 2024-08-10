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
  v <- expr <* char ';'
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

symbol :: Parser Expr
symbol = S <$> sym

erased :: Parser Expr
erased = char '\'' *> (Erased <$> expr)

parens :: Parser Expr
parens = char '[' *> expr <* char ']'

comment :: Parser ()
comment = (string "--" *> many (noneOf ['\n']) *> many (char '\n') $> ()) <|> (string "{- " *> right $> ())
  where right = try (string " -}") <|> (anyToken *> right)

expr :: Parser Expr
expr = do
  first <- ps
  op <- string ":>" <|> return ""
  case op of
    ":>" -> (first ::>) <$> ps
    "" -> app first <$> many ps
    o -> unexpected $ "Unexpeted operator " ++ show o ++ "."
    where
      p = erased <|> named <|> special <|> lambda <|> symbol <|> parens
      ps = spaces *> optional comment *> p <* optional comment <* spaces
      app f [] = f
      app f (x:xs) = app (f :@ x) xs

parse :: String -> Either ParseError Expr
parse = P.parse (expr <* eof) ""

parseShow :: Sym -> String -> IO ()
parseShow s e = case parse e of
  Right e' -> showLam s e'
  Left err -> putStrLn $ s ++ ": " ++ show err

parseExamples :: IO ()
parseExamples = do
  parseShow "zero" "(i: #L) -> (P: #U i) -> (s: (p: P) -> P) -> (z: P) -> z" 
  parseShow " l" "(i: #L) -> #i+0"
  parseShow "id" "(i: '#L) -> @Z = #U i; (T: 'Z) -> (tt: (t1: T) -> (t2: T) -> T) -> (t: T) -> @r = T; r :> @ttt = tt t; ttt t"
  parseShow " U" "#U +1"
  --             |         |         |         |         |         |         |         |         |         |         |         |
  --             0         10        20        30        40        50        60        70        80        90        100       110
