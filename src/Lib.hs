module Lib (module Lib) where

import Data.List ( (\\) )
import Control.Monad (unless)
import Prelude hiding (id)
import Data.Either (fromRight)

type Sym = String
data Expr
    = S Sym -- Symbol
    | Expr :@ Expr -- Application
    | Expr ::> Expr -- Left typing: Type ::> term
    | (Sym, Expr) :-> Expr -- Dependent lambda
    | U Sym Int -- Universe
    | L -- Type for unvirse level
    deriving (Eq, Read)
infixr 2 :->
infixl 8 :@
infix 6 ::>

instance Show Expr where
    show (S s) = s
    show ((S s) :@ x) = s ++ " " ++ show x
    show (f :@ x) = "(" ++ show f ++ ") " ++ show x
    show ((s, t) :-> e)
        | null s = show t ++ " -> " ++ show e
        | t == S "" = s ++ " -> " ++ show e
        | otherwise = "(" ++ s ++ " : " ++ show t ++ ") -> " ++ show e
    show (t ::> e) = show t ++ " :> " ++ show e
    show (U s i)
        | null s = "U " ++ show i
        | i == 0 = "U " ++ s
        | otherwise = "U " ++ s ++ "+" ++ show i
    show L = "L"

freeVars :: Expr -> [Sym]
freeVars (S s) = [s]
freeVars (f :@ a) = freeVars f ++ freeVars a
freeVars (t ::> e) = freeVars e ++ freeVars t
freeVars ((s, _t) :-> e) = freeVars e \\ [s]
freeVars (U l _) = [l]
freeVars L = []

whnf :: Expr -> Expr
whnf expr = spine expr []
    where
        spine (f :@ x) xs = spine f (x:xs)
        spine (_ ::> e) xs = spine e xs
        spine ((s, _t) :-> e) (x:xs) = spine (subst s x e) xs
        spine f xs = foldl (:@) f xs

subst :: Sym -> Expr -> Expr -> Expr
subst s x = sub
    where
        sub v@(S s') = if s == s' then x else v
        sub (f :@ x') = sub f :@ sub x'
        sub (t ::> e) = sub t ::> sub e
        sub ((s', t') :-> e')
          | s == s' = (s', sub t') :-> e'
          | s' `elem` fsx =
            let s'' = newSym e' s'
                e'' = substVar s' s'' e'
            in (s'', sub t') :-> sub e''
          | otherwise = (s', sub t') :-> sub e'
        sub ul@(U l i) = case x of
            S l' -> if s == l then U l' i else ul
            _ -> ul
        sub L = L

        fsx = freeVars x
        newSym e' s' = loop s'
            where
                loop s'' = if s'' `elem` vars then loop (s' ++ "'") else s''
                vars = fsx ++ freeVars e'

substVar :: Sym -> Sym -> Expr -> Expr
substVar s s' = subst s (S s')

alphaEq :: Expr -> Expr -> Bool
alphaEq (S s) (S s') = s == s'
alphaEq (f :@ x) (f' :@ x') = alphaEq f f' && alphaEq x x'
alphaEq (t ::> e) (t' ::> e') = alphaEq e e' && alphaEq t t'
alphaEq ((s, t) :-> e) ((s', t') :-> e') = alphaEq e (substVar s' s e') && alphaEq t t'
alphaEq (U l i) (U l' i') = l == l' && i == i'
alphaEq L L = True
alphaEq _ _ = False

nf :: Expr -> Expr
nf expr = spine expr []
    where
        spine (f :@ x) xs = spine f (x:xs)
        spine (t ::> e) [] = nf t ::> nf e
        spine (_ ::> e) xs = spine e xs
        spine ((s, t) :-> e) [] = (s, nf t) :-> nf e
        spine ((s, _t) :-> e) (x:xs) = spine (subst s x e) xs
        spine f xs = app f xs
        app f xs = foldl (:@) f (map nf xs)

betaEq :: Expr -> Expr -> Bool
betaEq e1 e2 = alphaEq (nf e1) (nf e2)

erased :: Expr -> Expr
erased (S s) = S s
erased (f :@ x) = erased f :@ erased x
erased (_t ::> e) = erased e
erased ((s, _t) :-> e) = (s, S "") :-> erased e
erased (U l i) = U l i
erased L = L

erasedBetaEq :: Expr -> Expr -> Bool
erasedBetaEq e1 e2 = alphaEq (erased . nf $ e1) (erased . nf $ e2)

newtype Env = Env [(Sym, Expr)] deriving (Show)

initialEnv :: Env
initialEnv = Env []

extend :: Env -> Sym -> Expr -> Env
extend (Env ls) s t = Env ((s, t) : ls)

type ErrorMsg = String
type TC a = Either ErrorMsg a
throwError :: String -> TC a
throwError = Left

findVar :: Env -> Sym -> TC Expr
findVar (Env ls) s =
    case lookup s ls of
    Just t -> return t
    Nothing -> throwError $ "Connat find variable " ++ s

tCheck :: Env -> Expr -> TC Expr
tCheck env (S s) = findVar env s
tCheck env (f :@ x) = do
    tf <- tCheck env f
    case tf of
        (s, t) :-> b -> do
            tx <- tCheck env x
            unless (betaEq tx t) $ throwError "Bad function argument type"
            return $ subst s x b
        e -> throwError $ "Non-function in application (" ++ show (f :@ x) ++ "): " ++ show e ++ "."
tCheck env (t ::> e) = do
    te <- tCheck env e
    unless (betaEq te t) $ throwError $ "Type missmatch (" ++ show te ++ "," ++ show t ++ ") in " ++ show (t ::> e) ++ "."
    return t
tCheck env ((s, t) :-> e) = do
    _ <- tCheck env t
    let env' = extend env s t
    te <- tCheck env' e
    return $ (s, t) :-> te
tCheck _ (U l i) = return $ U l (i+1)
tCheck _ L = return $ U "" 0

universe :: Env -> Expr -> TC (Sym, Int)
universe _ L = return ("", 0)
universe _ (U l i) = return (l, i+1)
universe env (S s) = findVar env s >>= universe env
universe env (f :@ _) = universe env f
universe env (_ ::> e) = universe env e
universe env ((s, t) :-> e) = universe (extend env s t) e

typeCheck :: Expr -> TC Expr
typeCheck = tCheck initialEnv

u :: Int -> Expr
u = U ""

id :: Expr
id = ("t", u 1) :-> ("x", S "t") :-> S "t" ::> S "x"
id' :: Expr
id' = ("r", u 1) :-> ("x", id :@ S "r") :-> S "x"
higher :: Expr
higher = ("f", ("t", u 1) :-> ("", S "t") :-> S "t") :-> S "f"
bigger :: Expr
bigger = ("f", ("t", u 1) :-> ("r", u 4) :-> S "r") :-> S "f"
level :: Expr
level = ("i", L) :-> ("T", U "i" 0) :-> ("t", S "T") :-> S "T" ::> S "t"

zero :: Expr
zero = ("a", u 1) :-> ("b", u 1) :-> ("s", S "a") :-> ("z", S "b") :-> S "z"
-- zero  = ("s", B) :. s
-- one = ("s", B :> B) :. ("z", B) :. s :@ z
-- two = ("s", B :> B) :. ("z", B) :. s :@ s :@ z
-- three = ("s", B :> B) :. ("z", B) :. s :@ s :@ s :@ z
-- plus = ("m", B :> B :> B) :. ("n", B :> B :> B) :. ("s", B :> B) :. ("z", B) :. m :@ s :@ (n :@ s :@ z)

showLam :: Sym -> Expr -> IO ()
showLam s lam = let lam' = nf lam in
     case typeCheck lam' of
         Right t -> do
             putStrLn $ s ++ " :: " ++ show t ++ " | " ++ show (case fromRight ("", -1) (universe initialEnv t) of (l, i) -> U l i)
             putStrLn $ s ++ " = " ++ show (erased lam')
         Left err -> print err

typeCheckVar :: Expr -> Expr -> TC Bool
typeCheckVar ty var = do
    tv <- typeCheck var
    return $ betaEq ty tv

someFunc :: IO ()
someFunc = do
    showLam "zero" zero
    showLam "id" id
    showLam "id'" id'
    showLam "higher" higher
    showLam "bigger" bigger
    showLam "r" $ u 1
    showLam "r" $ ("r", u 2) :-> S "r"
    showLam "test"  $ nf $ ("r", u 1) :-> ("l", S "r") :-> id :@ S "r" :@ S "l"
    showLam "level" level
    print $ betaEq (("", S "t") :-> S "t") (("", S "t") :-> S "t")
    print $ typeCheckVar (("t", u 1) :-> ("", S "t") :-> S "t") (("r", u 1) :-> ("x", S "r") :-> S "x")

