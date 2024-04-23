module Lib where

import Data.List ( (\\) )
import Control.Monad (unless)
import Prelude hiding (id)
import Data.Either (fromRight)

type Sym = String
data Expr
    = S Sym
    | Expr :@ Expr
    | Expr ::: Expr
    | (Sym, Expr) :. Expr
    | U Int
    deriving (Eq, Read)
infixr 2 :.
infixl 8 :@
infix 6 :::

instance Show Expr where
    show (S s) = s
    show ((S s) :@ x) = s ++ " " ++ show x
    show (f :@ x) = "(" ++ show f ++ ") " ++ show x
    show ((s, t) :. e)
        | null s = show t ++ " -> " ++ show e
        | otherwise = "(" ++ s ++ " : " ++ show t ++ ") -> " ++ show e
    show (e ::: t) = show e ++ " : " ++ show t
    show (U i) = "U" ++ show i

freeVars :: Expr -> [Sym]
freeVars (S s) = [s]
freeVars (f :@ a) = freeVars f ++ freeVars a
freeVars (e ::: t) = freeVars e ++ freeVars t
freeVars ((s, _t) :. e) = freeVars e \\ [s]
freeVars (U _) = []

whnf :: Expr -> Expr
whnf expr = spine expr []
    where
        spine (f :@ x) xs = spine f (x:xs)
        spine (e ::: _) xs = spine e xs
        spine ((s, _t) :. e) (x:xs) = spine (subst s x e) xs
        spine f xs = foldl (:@) f xs

subst :: Sym -> Expr -> Expr -> Expr
subst s x = sub
    where
        sub v@(S s') = if s == s' then x else v
        sub (f :@ x') = sub f :@ sub x'
        sub (e ::: t) = sub e ::: sub t
        sub ((s', t') :. e')
          | s == s' = (s', sub t') :. e'
          | s' `elem` fsx =
            let s'' = newSym e' s'
                e'' = substVar s' s'' e'
            in (s'', sub t') :. sub e''
          | otherwise = (s', sub t') :. sub e'
        sub u@(U _) = u

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
alphaEq (e ::: t) (e' ::: t') = alphaEq e e' && alphaEq t t'
alphaEq ((s, t) :. e) ((s', t') :. e') = alphaEq e (substVar s' s e') && alphaEq t t'
alphaEq (U a) (U b) = a == b
alphaEq _ _ = False

nf :: Expr -> Expr
nf expr = spine expr []
    where
        spine (f :@ x) xs = spine f (x:xs)
        spine (e ::: t) [] = nf e ::: nf t
        spine (e ::: _) xs = spine e xs
        spine ((s, t) :. e) [] = (s, nf t) :. nf e
        spine ((s, _t) :. e) (x:xs) = spine (subst s x e) xs
        spine f xs = app f xs
        app f xs = foldl (:@) f (map nf xs)

betaEq :: Expr -> Expr -> Bool
betaEq e1 e2 = alphaEq (nf e1) (nf e2)

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
        (s, t) :. b -> do
            tx <- tCheck env x
            unless (betaEq tx t) $ throwError "Bad function argument type"
            return $ subst s x b
        e -> throwError $ "Non-function in application (" ++ show (f :@ x) ++ "): " ++ show e ++ "."
tCheck env (e ::: t) = do
    te <- tCheck env e
    unless (betaEq te t) $ throwError $ "Type missmatch (" ++ show te ++ "," ++ show t ++ ") in " ++ show (e ::: t) ++ "."
    return t
tCheck env ((s, t) :. e) = do
    _ <- tCheck env t
    let env' = extend env s t
    te <- tCheck env' e
    return $ (s, t) :. te
tCheck _ (U a) = return $ U (a+1)

universe :: Env -> Expr -> TC Int
universe _ (U i) = return (i+1)
universe env (S s) = findVar env s >>= universe env
universe env (f :@ _) = universe env f
universe env (e ::: _) = universe env e
universe env ((s, t) :. e) = universe (extend env s t) e

typeCheck :: Expr -> TC Expr
typeCheck = tCheck initialEnv



id :: Expr
id = ("t", U 1) :. ("x", S "t") :. S "x" ::: S "t"
id' :: Expr
id' = ("r", U 1) :. ("x", id :@ S "r") :. S "x"
higher :: Expr
higher = ("f", ("t", U 1) :. ("", S "t") :. S "t") :. S "f"
bigger :: Expr
bigger = ("f", ("t", U 1) :. ("r", U 4) :. S "r") :. S "f"

zero :: Expr
zero = ("a", U 1) :. ("b", U 1) :. ("s", S "a") :. ("z", S "b") :. S "z"
-- zero  = ("s", B) :. s
-- one = ("s", B :> B) :. ("z", B) :. s :@ z
-- two = ("s", B :> B) :. ("z", B) :. s :@ s :@ z
-- three = ("s", B :> B) :. ("z", B) :. s :@ s :@ s :@ z
-- plus = ("m", B :> B :> B) :. ("n", B :> B :> B) :. ("s", B :> B) :. ("z", B) :. m :@ s :@ (n :@ s :@ z)

showLam :: Sym -> Expr -> IO ()
showLam s l' = let l = nf l' in
    case typeCheck l of
        Right t -> do
            putStrLn $ s ++ " :: " ++ show t ++ " | U" ++ show (fromRight (-1) (universe initialEnv t))
            putStrLn $ s ++ " " ++ show l
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
    showLam "r" $ U 1
    showLam "r" $ ("r", U 2) :. S "r"
    showLam "test"  $ nf $ ("r", U 1) :. ("l", S "r") :. id :@ S "r" :@ S "l"
    print $ betaEq (("", S "t") :. S "t") (("", S "t") :. S "t")
    print $ typeCheckVar (("t", U 1) :. ("", S "t") :. S "t") (("r", U 1) :. ("x", S "r") :. S "x")

