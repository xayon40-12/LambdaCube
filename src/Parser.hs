{-# LANGUAGE TupleSections #-}
module Parser (parse, parseShow, parseExamples) where

import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec hiding (parse)
import Lib (Sym, Expr (..), Levels, showLam, AsT (..), Erased, app, app')
import Data.Functor
import Data.Map.Strict (fromList)
import qualified Text.Parsec as P
import Data.Foldable (foldl')

type Pos = (Line, Column)
data Info = Info {
    infoName :: String,
    infoStart :: Pos,
    infoEnd :: Pos
} deriving Show

info :: SourcePos -> Parser Info
info s = do
  e <- getPosition
  let e' = (sourceLine e, sourceColumn e)
  return $ Info file s' e'
  where
    file = sourceName s
    s' = (sourceLine s, sourceColumn s)

sym :: Parser Sym
sym = do
  l <- letter
  ls <- many alphaNum
  return $ l:ls

named :: Parser (Expr Info)
named = do
  s <- char '@' *> sym <* spaces <* char '='
  v <- expr <* char ';' <* spaces <* comments
  (Let s v <$> expr) <|> return v

special :: Parser (Expr Info)
special = char '#' *> (levelT <|> universe <|> level)

levelT :: Parser (Expr Info)
levelT = char 'L' $> LevelT

lv :: Parser (Sym, Int)
lv = do
  s <- sym <* char '+'
  i <- read <$> many1 digit
  return (s, i)

level :: Parser (Expr Info)
level = uncurry Level <$> lv

levels :: Parser Levels
levels = fromList <$> sepBy1 (try lv <|> ((,0) <$> sym)) (char ',')

universe :: Parser (Expr Info)
universe = Universe <$> (char 'U' *> space *> levels)

erased :: Parser Erased
erased = char '\'' $> True <|> return False

introduce :: Parser (Expr Info)
introduce = do
  pstart <- getPosition
  s <- char '(' *> sym
  t <- char ':' *> expr
  let
    lambda = do
      er <- char ')' *> spaces *> erased
      e <- string "->" *> expr
      i <- info pstart
      return $ Lam i (er, s, t) e
    intersection = do
      t2 <- string "/\\" *> expr <* char ')'
      return $ InterT (s, t) t2
  lambda <|> intersection

symbol :: Parser (Expr Info)
symbol = Symbol <$> sym

parens :: Parser (Expr Info)
parens = char '[' *> expr <* char ']'

comment :: Parser String
comment = end <|> inner
  where
    end = mappend <$> try (string "--") <*> many (noneOf ['\n'])
    inner = mappend <$> try (string "{- ") <*> ((mappend <$> inner <*> innerRight) <|> innerRight)
    innerRight = try (string " -}") <|> ((:) <$> anyToken <*> innerRight)

comments :: Parser [String]
comments = many (comment <* spaces)

surroundCommentSpaces :: Parser (Expr Info) -> Parser (Expr Info)
surroundCommentSpaces p = spaces *> comments *> p <* spaces <* comments

baseExpr :: Parser (Expr Info)
baseExpr = named <|> special <|> introduce <|> symbol <|> parens

postExpr :: Parser (Expr Info)
postExpr = foldl' (.) surroundCommentSpaces posts baseExpr
  where
    postInter = post ".1" (`As` One) . post ".2" (`As` Two)
    posts = [postInter]

opExpr :: Parser (Expr Info)
opExpr = foldl' (.) id ops postExpr
    where
      opApp = associate ALeft [("", app), ("'", app')]
      opIntersect = associate ARight [("^", Inter)]
      opType = associate ANone [(":>", Typed)]
      ops = [opType, opIntersect, opApp]

expr :: Parser (Expr Info)
expr = opExpr

data Associate = ALeft | ARight | ANone

post :: String -> (Expr Info -> Expr Info) -> Parser (Expr Info) -> Parser (Expr Info)
post s op p = do
  e <- p
  (try (string s) $> op e) <|> return e

associate :: Associate -> [(String, Expr Info -> Expr Info -> Expr Info)] -> Parser (Expr Info) -> Parser (Expr Info)
associate a nops p = do
  l <- p
  r <- many (things nops)
  go a l r
  where
    things [] = unexpected ""
    things ((n,op):nops') = (try (string n) *> p <&> (op,)) <|> things nops'
    go _ f [] = return f
    go _ f [(op,x)] = return  $ f `op` x
    go ANone _f _xs = unexpected $ "operators \"" ++ show (fst <$> nops) ++ "\" are not associative."
    go ALeft f ((op,x):xs) = go ALeft (f `op` x) xs
    go ARight f ((op,x):xs) = op f <$> go ARight x xs

parse :: String -> String -> Either ParseError (Expr Info)
parse = P.parse (expr <* eof)

parseShow :: Sym -> String -> IO ()
parseShow s e = case parse s e of
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
