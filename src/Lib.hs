module Lib
    ( someFunc
    ) where
import Data.List ( (\\) )

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Sym = String
data Expr
    = Var Sym
    | App Expr Expr
    | Lam Sym Expr
    deriving (Eq, Read, Show)

whnf :: Expr -> Expr
whnf expr = spine expr []
    where
        spine (App f x) xs = spine f (x:xs)
        spine (Lam s e) (x:xs) = spine (subst s x e) xs
        spine f xs = foldl App f xs

freeVars :: Expr -> [Sym]
freeVars (Var s) = [s]
freeVars (App f a) = freeVars f ++ freeVars a
freeVars (Lam s e) = freeVars e \\ [s]

subst :: Sym -> Expr -> Expr -> Expr
subst s x e = sub e
    where
        sub v@(Var s') = if s == s' then x else v
        sub (App f x') = App (sub f) (sub x')
        sub l@(Lam s' e')
          | s == s' = l
          | s' `elem` fsx =
            let s'' = newSym e' s'
                e'' = substVar s' s'' e'
            in Lam s'' (sub e'')
          | otherwise = Lam s' (sub e')
        fsx = freeVars x
        newSym e' s' = loop s'
            where
                loop s'' = if s'' `elem` vars then loop (s' ++ "'") else s''
                vars = fsx ++ freeVars e'

substVar :: Sym -> Sym -> Expr -> Expr
substVar s s' e = subst s (Var s') e

alphaEq :: Expr -> Expr -> Bool
alphaEq (Var s) (Var s') = s == s'
alphaEq (App f x) (App f' x') = alphaEq f f' && alphaEq x x'
alphaEq (Lam s e) (Lam s' e') = alphaEq e (substVar s' s e')
alphaEq _ _ = False

nf :: Expr -> Expr
nf expr = spine expr []
    where
        spine (App f x) xs = spine f (x:xs)
        spine (Lam s e) [] = Lam s (nf e)
        spine (Lam s e) (x:xs) = spine (subst s x e) xs
        spine f xs = app f xs
        app f xs = foldl App f (map nf xs)

betaEq :: Expr -> Expr -> Bool
betaEq e1 e2 = alphaEq (nf e1) (nf e2)

[z,s,m,n] = map (Var . (:[])) "zsmn"
app2 f x y = App (App f x) y
zero  = Lam "s" $ Lam "z" z
one   = Lam "s" $ Lam "z" $ App s z
two   = Lam "s" $ Lam "z" $ App s $ App s z
three = Lam "s" $ Lam "z" $ App s $ App s $ App s z
plus  = Lam "m" $ Lam "n" $ Lam "s" $ Lam "z" $ app2 m s (app2 n s z) -- Should be True
