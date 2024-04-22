module Lib where

import Data.List ( (\\) )
import Control.Monad (unless)
import Prelude hiding (id)

data Kinds = Star | Box deriving (Eq, Read, Show)
type Sym = String
data Expr
    = Var Sym
    | Expr :@ Expr
    | (Sym, Expr) :. Expr
    | Kind Kinds
    deriving (Eq, Read, Show)
infixr 2 :.
infixr 3 :@
infixr 3 .:
(.:) :: (Sym, Expr) -> Sym -> Expr
l .: s = l :. Var s

freeVars :: Expr -> [Sym]
freeVars (Var s) = [s]
freeVars (f :@ a) = freeVars f ++ freeVars a
freeVars ((s, _t) :. e) = freeVars e \\ [s]
freeVars (Kind _) = []

whnf :: Expr -> Expr
whnf expr = spine expr []
    where
        spine (f :@ x) xs = spine f (x:xs)
        spine ((s, _t) :. e) (x:xs) = spine (subst s x e) xs
        spine f xs = foldl (:@) f xs

subst :: Sym -> Expr -> Expr -> Expr
subst s x = sub
    where
        sub v@(Var s') = if s == s' then x else v
        sub (f :@ x') = sub f :@ sub x'
        sub ((s', t') :. e')
          | s == s' = (s', sub t') :. e'
          | s' `elem` fsx =
            let s'' = newSym e' s'
                e'' = substVar s' s'' e'
            in (s'', sub t') :. sub e''
          | otherwise = (s', sub t') :. sub e'
        sub (Kind k) = Kind k

        fsx = freeVars x
        newSym e' s' = loop s'
            where
                loop s'' = if s'' `elem` vars then loop (s' ++ "'") else s''
                vars = fsx ++ freeVars e'

substVar :: Sym -> Sym -> Expr -> Expr
substVar s s' = subst s (Var s')

alphaEq :: Expr -> Expr -> Bool
alphaEq (Var s) (Var s') = s == s'
alphaEq (f :@ x) (f' :@ x') = alphaEq f f' && alphaEq x x'
alphaEq ((s, t) :. e) ((s', t') :. e') = alphaEq e (substVar s' s e') && alphaEq t t'
alphaEq (Kind a) (Kind b) = a == b
alphaEq _ _ = False

nf :: Expr -> Expr
nf expr = spine expr []
    where
        spine (f :@ x) xs = spine f (x:xs)
        spine ((s, t) :. e) [] = (s, t) :. nf e
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
tCheck env (Var s) = findVar env s
tCheck env (f :@ x) = do
    tf <- tCheck env f
    case tf of
        (s, t) :. b -> do
            tx <- tCheck env x
            -- the magic is that as the Pi type depends of the variable of the lambda it is the type of, its own type must be the same as the lambda
            unless (betaEq tx t) $ throwError "Bad function argument type"
            return $ subst s x b
        e -> throwError $ "Non-function in application: " ++ show e ++ "."
tCheck env ((s, t) :. e) = do
    tt <- tCheck env t -- check that the kind is valid
    let env' = extend env s t
    tse <- tCheck env' $ subst s t e
    unless ((tt, tse) `elem` allowedKinds) $ throwError $ "Bad abstraction (" ++ show tt ++ "," ++ show tse ++ ") in checking " ++ show ((s, t):.e) ++ "."
    return tse
tCheck _ (Kind Star) = return $ Kind Box
tCheck _ (Kind Box) = throwError "Found Box" -- Universes could be used instead of throwing error

ks :: Expr
ks = Kind Star
kb :: Expr
kb = Kind Box
allowedKinds :: [(Expr, Expr)]
allowedKinds = [(ks, ks), (ks, kb), (kb, ks), (kb, kb)]

typeCheck :: Expr -> TC Expr
typeCheck = tCheck initialEnv



higher = ("f", ("t", ks) :. ("", Var "t") .: "t") .: "f"
id = ("t", ks) :. ("x", Var "t") .: "x"
zero = ("s", ks) .: "s"
-- zero  = ("s", B) :. s
-- one = ("s", B :> B) :. ("z", B) :. s :@ z
-- two = ("s", B :> B) :. ("z", B) :. s :@ s :@ z
-- three = ("s", B :> B) :. ("z", B) :. s :@ s :@ s :@ z
-- plus = ("m", B :> B :> B) :. ("n", B :> B :> B) :. ("s", B :> B) :. ("z", B) :. m :@ s :@ (n :@ s :@ z)

someFunc :: IO ()
someFunc = do
    print id
    print $ typeCheck id
    print $ typeCheck zero
    print $ typeCheck higher
    print $ typeCheck $ zero :@ ks
    print $ typeCheck $ id :@ ks

