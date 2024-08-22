module Lib (module Lib) where

import Data.List (intercalate)
import Control.Monad (unless)
import Prelude hiding (id)
import Data.Either (fromRight)
import Data.Map.Strict (Map, singleton, insert, delete, toList, keys)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Bifunctor (second)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Function (id)

type Sym = String
type Levels = Map Sym Int
data Inter = One | Two deriving Eq
type Type = Expr
data Expr
    = L -- Type for unvirse level
    | Sym :+ Int -- Constructor of a level
    | U Levels -- Universe where the level is the max of each Sym+Int
    | (Sym, Type) :-> Expr -- Dependent lambda
    | Expr :@ Expr -- Application
    | (Sym, Expr) :/\ Expr -- Depedent intersection type
    | Expr :^ Expr -- Dependent intersection term
    | I Expr Inter -- Term of a dependent intersection with either the first or second type depending on the value of `Inter`
    | Type ::> Expr -- Left typing: Type ::> term
    | E Sym Expr Expr
    | S Sym -- Symbol
    | Erased Expr
    deriving Eq
infixr 2 :->
infixl 7 :@
infix 6 ::>
infix 8 :+

instance Show Inter where
    show One = "1"
    show Two = "2"

instance Show Expr where
    show L = "#L"
    show (s :+ i) = s ++ "+" ++ show i
    show (U l) = "#U " ++ showL l
    -- show ((_s, Erased _t) :-> e) = show e
    show ((s, t) :-> e)
        | null s = show t ++ " -> " ++ show e
        | t == S "" = s ++ " -> " ++ show e
        | otherwise = "(" ++ s ++ ": " ++ show t ++ ") -> " ++ show e
    show (f@((_, _) :-> _) :@ x) = "[" ++ show f ++ "] " ++ showApp x
    -- show (f :@ Erased _x) = show f
    show (f :@ x) = show f ++ " " ++ showApp x
    show (t ::> e) = show t ++ " :> " ++ show e
    show ((s, t1) :/\ t2) = s ++ ": " ++ show t1 ++ " /\\ " ++ show t2
    show (e1 :^ e2) = show e1 ++ " ^ " ++ show e2
    show (I e i) = show e ++ "." ++ show i
    show (E s v e) = "@" ++ s ++ " = " ++ show v ++ "; " ++ show e
    show (S s) = s
    show (Erased e) = "'" ++ show e

showApp :: Expr -> String
showApp (f :@ x) = "[" ++ show f ++ " " ++ showApp x ++ "]"
showApp (t ::> x) = "[" ++ show t ++ " :> " ++ showApp x ++ "]"
showApp e = show e

showL :: Levels -> String
showL m = showm (toList m)
    where
        showm [] = ""
        showm [(l, i)] = showU l i
        showm lis = "max(" ++ intercalate "," (uncurry showU <$> lis) ++ ")"

showU :: Sym -> Int -> String
showU l i
    | null l =  show i
    | i == 0 =  l
    | otherwise =  l ++ "+" ++ show i

freeVars :: Expr -> Set Sym
freeVars L = Set.empty
freeVars (s :+ _) = Set.singleton s
freeVars (U ls) = Set.fromList $ keys ls
freeVars ((s, t) :-> e) = freeVars t <> Set.delete s (freeVars e)
freeVars (f :@ a) = freeVars f <> freeVars a
freeVars ((s, t1) :/\ t2) = freeVars t1 <> Set.delete s (freeVars t2)
freeVars (e1 :^ e2) = freeVars e1 <> freeVars e2
freeVars (I e _i) = freeVars e
freeVars (t ::> e) = freeVars t <> freeVars e
freeVars (E s v e) = freeVars v <> Set.delete s (freeVars e)
freeVars (S s) = Set.singleton s
freeVars (Erased e) = freeVars e

nf :: Expr -> Expr
nf expr = spine expr []
    where
        spine ((s, t) :-> e) [] = (s, nf t) :-> nf e
        spine ((s, _t) :-> e) (Erased x:xs) = spine (subst s (unErase x) e) xs
        spine ((s, _t) :-> e) (x:xs) = spine (subst s x e) xs
        spine (f :@ x) xs = spine f (x:xs)
        spine ((s, t1) :/\ t2) [] = (s, nf t1) :/\ nf t2
        spine (e1 :^ e2) [] = nf e1 :^ nf e2
        spine (I e i) [] = I (nf e) i
        spine (t ::> e) [] = nf t ::> nf e
        spine (E s v e) xs = spine (subst s v e) xs
        spine (Erased e) [] = Erased (nf e)
        spine (Erased e) xs = Erased (spine e xs)
        spine f xs = app f xs
        app f xs = foldl (:@) f (map nf xs)
whnf :: Expr -> Expr
whnf expr = spine expr []
    where
        spine ((s, _t) :-> e) (Erased x:xs) = spine (subst s (unErase x) e) xs
        spine ((s, _t) :-> e) (x:xs) = spine (subst s x e) xs
        spine (f :@ x) xs = spine f (x:xs)
        spine (t ::> e) [] = whnf t ::> whnf e
        spine (E s v e) xs = spine (subst s v (whnf e)) xs
        spine (Erased e) [] = Erased (whnf e)
        spine f xs = app f xs
        app f xs = foldl (:@) f (map nf xs)

subst :: Sym -> Expr -> Expr -> Expr
subst "" _x = id -- if the symbol is empty, no substitution is done
subst s x = sub
    where
        sub L = L
        sub l@(s' :+ i) = case x of
            S l' -> if s' == s then l' :+ i else l
            s'' :+ i' -> if s' == s then s'' :+ (i+i') else l
            _ -> l
        sub ul@(U ls) = case x of
            S l -> case Map.lookup s ls of
                Just i -> U (insert l i $ delete s ls)
                Nothing -> ul
            s' :+ i' -> case Map.lookup s ls of
                Just i -> U (insert s' (i+i') $ delete s ls)
                Nothing -> ul
            _ -> ul
        sub ((s', t') :-> e')
          | s == s' = (s', sub t') :-> e'
          | s' `elem` fsx =
            let s'' = newSym e' s'
                e'' = substVar s' s'' e'
            in (s'', sub t') :-> sub e''
          | otherwise = (s', sub t') :-> sub e'
        sub (f :@ x') = sub f :@ sub x'
        sub ((s', t1) :/\ t2)
          | s == s' = (s', sub t1) :/\ t2
          | s' `elem` fsx =
            let s'' = newSym t2 s'
                e'' = substVar s' s'' t2
            in (s'', sub t1) :/\ sub e''
          | otherwise = (s', sub t1) :/\ sub t2
        sub (e1 :^ e2) = sub e1 :^ sub e2
        sub (I e i) = I (sub e) i
        sub (t ::> e) = sub t ::> sub e
        sub (E s' v e')
          | s == s' = E s' (sub v) e'
          | s' `elem` fsx =
            let s'' = newSym e' s'
                e'' = substVar s' s'' e'
            in E s'' (sub v) (sub e'')
          | otherwise = E s' (sub v) (sub e')
        sub v@(S s') = if s == s' then x else v
        sub (Erased e) =  Erased (sub e)

        fsx = freeVars x
        newSym e' s' = loop s'
            where
                loop s'' = if s'' `elem` vars then loop (s' ++ "'") else s''
                vars = fsx <> freeVars e'

substVar :: Sym -> Sym -> Expr -> Expr
substVar s s' = subst s (S s')

data Direction = Symmetric | Directional IsType

alphaEq :: Expr -> Expr -> Bool
alphaEq L L = True
alphaEq (s :+ i) (s' :+ i') = s == s' && i == i'
alphaEq (U ls) (U ls') = ls == ls'
alphaEq ((s, t) :-> e) ((s', t') :-> e') = alphaEq e (substVar s' s e') && alphaEq t t'
alphaEq (f :@ x) (f' :@ x') = alphaEq f f' && alphaEq x x'
alphaEq ((s, t1) :/\ t2) ((s', t1') :/\ t2') = alphaEq t2 (substVar s' s t2') && alphaEq t1 t1'
alphaEq (e1 :^ e2) (e1' :^ e2') = alphaEq e1 e1' && alphaEq e2 e2'
alphaEq (I e i) (I e' i') = alphaEq e e' && i == i'
alphaEq (t ::> e) (t' ::> e') = alphaEq e e' && alphaEq t t'
alphaEq (E s v e) e' = alphaEq (subst s v e) e'
alphaEq e (E s v e') = alphaEq e (subst s v e')
alphaEq (S s) (S s') = s == s'
alphaEq (Erased e) (Erased e') = alphaEq e e'
alphaEq _ _ = False

-- first value correspond to the expected level, so the largest one in the case of cumulative universes
universeValid :: Levels -> Levels -> Bool
universeValid l1 l2 = fromMaybe False (req (toList l2))
    where
        req [] = Nothing
        req [l] = iner l
        req (l:ls) = iner l >>= \b -> (b||) <$> req ls
        iner (l, i) = (i<=) <$> Map.lookup l l1

betaEq :: Expr -> Expr -> Bool
betaEq e1 e2 = alphaEq (nf e1) (nf e2)

erased :: Expr -> Expr
erased L = L
erased (s :+ i) = s :+ i
erased (U ls) = U ls
erased ((_, Erased _) :-> e) = erased e
erased ((s, _t) :-> e) = (s, S "") :-> erased e
erased (f :@ (Erased _)) = erased f
erased (f :@ x) = erased f :@ erased x
erased ((s, t1) :/\ t2) = (s, erased t1) :/\ erased t2
erased (e1 :^ _e2) = erased e1
erased (I e _i) = erased e
erased (_t ::> e) = erased e
-- erased (E s v e) = E s (erased v) (erased e)
erased (E s v e) = erased $ subst s v e
erased (S s) = S s
erased (Erased e) = Erased e

erasedBetaEq :: Expr -> Expr -> Bool
erasedBetaEq e1 e2 = alphaEq (erased . nf $ e1) (erased . nf $ e2)

newtype Env = Env (Map Sym Expr) deriving (Show)

initialEnv :: Env
initialEnv = Env Map.empty

extend :: Env -> Sym -> Expr -> Env
extend (Env ls) s t = Env $ Map.insert s t ls

type ErrorMsg = String
type TC a = Either ErrorMsg a
throwError :: String -> TC a
throwError = Left

findVar :: Env -> Sym -> TC (IsType, Expr)
findVar (Env ls) s =
    case Map.lookup s ls of
        Just t -> second (const (nf t)) <$> tCheck (Env ls) t
        Nothing -> throwError $ "Cannot find variable " ++ s

unTyped :: Expr -> Expr
unTyped (_t ::> e) = unTyped e
unTyped e = e

validTyping :: Env -> Expr -> Expr -> Expr -> Bool
validTyping env vt1 vt2 ve2 = valid (nf vt1) (nf vt2)
    where
        valid L L = True
        valid (s :+ i) (s' :+ i') = s == s' && i >= i'
        valid (U ls) (U ls') = universeValid ls ls'
        valid (U ls) e = either (const False) (universeValid ls) (universe env e)
        valid ((s, t) :-> e) ((s', t') :-> e') = valid e (substVar s' s e') && valid t t'
        valid (f :@ x) (f' :@ x') = valid f f' && valid x x'
        valid ((s, t1) :/\ t2) ((s', t1') :/\ t2') = case nf ve2 of
            e1 :^ _e2 -> valid (subst s e1 t2) t2' && valid t1 t1'
            _ -> valid t2 (substVar s' s t2') && valid t1 t1'
        valid (e1 :^ e2) (e1' :^ e2') = valid e1 e1' && valid e2 e2'
        valid (I e i) (I e' i') = valid e e' && i == i'
        valid e (_t' ::> e') = valid e e'
        valid (_t ::> e) e' = valid e e'
        valid (E s v e) e' = valid (subst s v e) e'
        valid e (E s v e') = valid e (subst s v e')
        valid (S s) (S s') = s == s'
        valid (Erased e) (Erased e') = valid e e'
        valid _ _ = False

universe :: Env -> Expr -> TC Levels
universe _ L = return $ singleton "" 0
universe _ (s :+ i) = throwError $ "The term \"" ++ s ++ "+" ++ show i ++ ")\" does not have a universe."
universe _ (U ls) = return ((+1) <$> ls)
universe env ((s, t) :-> e) = do
    u1 <- universe env t
    u2 <- universe (extend env s t) e
    return $ maxLevel u1 u2
universe env (f :@ _) = universe env f
universe env ((s, t1) :/\ t2) = do
    u1 <- universe env t1
    u2 <- universe (extend env s t1) t2
    return $ maxLevel u1 u2
universe env (e1 :^ e2) = do
    u1 <- universe env e1
    u2 <- universe env e2
    return $ maxLevel u1 u2
universe env (I e _i) = universe env e
universe env (_ ::> e) = universe env e
universe env (E s v e) = universe env (subst s v e)
universe env (S s) = findVar env s >>= ((((+(-1)) <$>) <$>) . universe env) . snd
universe env (Erased e) = universe env e

sameErasure :: Expr -> Expr -> Bool
sameErasure (Erased _) (Erased _) = True
sameErasure (Erased _) _ = False
sameErasure _ (Erased _) = False
sameErasure _ _ = True

addErased :: Expr -> Expr
addErased (Erased e) = Erased e
addErased e = Erased e

unErase :: Expr -> Expr
unErase (Erased e) = e
unErase e = e

propagateErase :: Expr -> Expr
propagateErase (Erased ((s, t) :-> e)) = (s, Erased t) :-> Erased e
propagateErase e = e

type IsType = Maybe Levels
tCheck :: Env -> Expr -> TC (IsType, Expr)
tCheck _ L = let l = singleton "" 0 in return (Just l, U l)
tCheck env (s :+ _i) = findVar env s
tCheck _ (U ls) = let l = (+1) <$> ls in return (Just l, U l)
tCheck env ((s, t) :-> e) = do
    (isT, tt) <- tCheck env t
    case isT of
        Nothing -> throwError $ "The type of a type must be a universe, which is not the case for \"[" ++ s ++ ": " ++ show t ++ "]: " ++ show tt ++ "\"."
        Just l -> do
            let env' = extend env s t
            (isT', te) <- tCheck env' e
            return (maxLevel l <$> isT', (s, t) :-> te)
tCheck env (f :@ x) = do
    (isT, tf) <- tCheck env f
    (_isT', tx) <- tCheck env x
    case propagateErase . unTyped . whnf $ tf of
        (s, t) :-> b -> do
            unless (sameErasure t x) $ throwError $ "In application the erasure must match and be specified manually, erasure of \"" ++ show t ++ "\" and \"" ++ show x ++ "\" differ"
            unless (validTyping env t tx x) $ throwError $ "Bad function argument type:\n" ++ show (nf t) ++ ": U " ++ show (universe env t) ++ ",\n" ++ show (nf tx) ++ ": U " ++ show (universe env tx) ++ "\nin " ++ show (f :@ x) ++ "."
            return (isT, subst s (unErase x) b)
        e -> throwError $ "Non-function in application (" ++ show (f :@ x) ++ "): " ++ show e ++ "."
tCheck env ((s, t1) :/\ t2) = do
    (isT, tt1) <- tCheck env t1
    case isT of
        Nothing -> throwError $ "The type of a type must be a universe, which is not the case for \"[" ++ s ++ ": " ++ show t1 ++ "]: " ++ show tt1 ++ "\"."
        Just l -> do
            let env' = extend env s t1
            (isT', tt2) <- tCheck env' t2
            return (maxLevel l <$> isT', (s, t1) :/\ tt2)
tCheck env (e1 :^ e2) = do
    unless (erasedBetaEq e1 e2) $ throwError $ "To construct a term of an intersection, one term of each types must be provided and have the same erasure. The term:\n" ++ show e1 ++ "\nand\n" ++ show e2 ++ "\nhave different erasure:\n" ++ show (erased e1) ++ ",\n" ++ show (erased e2)
    (isT1, t1) <- tCheck env e1
    (isT2, t2) <- tCheck env e2
    return (maxLevel <$> isT1 <*> isT2, ("", t1) :/\ t2)
tCheck env (I e i) = do
    (isT, te) <- tCheck env e
    case te of
        ((s, t1) :/\ t2) -> case i of 
            One -> return (isT, t1)
            Two -> return (isT, subst s (I e One) t2)
        t -> throwError $ "The post-fix operator to access a dependent intersection must be applied to a term whose type is a dependent intersection, provided:\n" ++ show (I e i) ++ ": " ++ show t
tCheck env (t ::> e) = do
    _ <- tCheck env t
    (isT, te) <- tCheck env e
    unless (validTyping env t te e) $ throwError $ "Type missmatch:\n" ++ show (nf t) ++ ",\n" ++ show (nf te) ++ "\n." --"in " ++ show (t ::> e) ++ "."
    return (isT, t)
tCheck env (E s v e) = tCheck env v *> tCheck env (subst s v e)
tCheck env (S s) = findVar env s
tCheck env (Erased e) = second addErased <$> tCheck env e

maxLevel :: Levels -> Levels -> Levels
maxLevel l1 l2 = lmax l1 (toList l2)
    where
        lmax acc [] = acc
        lmax acc ((l, i):ls) = lmax (Map.insertWith max l i acc) ls

typeCheck :: Expr -> TC Expr
typeCheck = (snd <$>) . tCheck initialEnv

showLam :: Sym -> Expr -> IO ()
showLam s lam = do
    putStrLn $ s ++ ":\n" ++ show lam
    putStrLn $ s ++ ":\n" ++ show (erased lam)
    case typeCheck lam of
        Right t -> do
         putStrLn $ s ++ " :: " ++ show t ++ " | " ++ show (case fromRight (singleton "ERROR" (-1)) (universe initialEnv t) of ls -> U ls)
         putStrLn $ s ++ " e.nf= " ++ show (erased . nf $ lam)
        Left err -> putStrLn $ s ++ ": " ++ err

typeCheckVar :: Expr -> Expr -> TC Bool
typeCheckVar ty var = do
    let env = initialEnv
    (_isT, tv) <- tCheck env var
    return $ validTyping env ty tv var

lv :: Sym -> Int -> Levels
lv = singleton

ui :: Int -> Expr
ui i = U (singleton "" i)
us :: Sym -> Expr
us l = U (singleton l 0)
u :: Sym -> Int -> Expr
u l i = U (singleton l i)

id' :: Expr
id' = ("i", L) :-> ("t", us "i") :-> ("x", S "t") :-> S "t" ::> S "x"
higher :: Expr
higher = ("f", ("t", ui 1) :-> ("", S "t") :-> S "t") :-> S "f"
bigger :: Expr
bigger = ("f", ("t", ui 1) :-> ("r", ui 4) :-> S "r") :-> S "f"
level :: Expr
level = ("i", L) :-> ("T", us "i") :-> ("t", S "T") :-> S "T" ::> S "t"
false :: Expr
false = ("i", L) :-> ("T", us "i") :-> us "i" ::> S "T"
maxleveli :: Expr
maxleveli = ("i", L) :-> ("Ti", us "i") :-> ("j", L) :-> ("Tj", us "j") :-> U (maxLevel (lv "i" 0) (lv "j" 0)) ::> S "Ti"
maxlevelj :: Expr
maxlevelj = ("i", L) :-> ("Ti", us "i") :-> ("j", L) :-> ("Tj", us "j") :-> U (maxLevel (lv "i" 0) (lv "j" 0)) ::> S "Tj"
invalideLevel :: Expr
invalideLevel = ("T", ui 0) :-> ("t", S "T") :-> ("w", S "t") :-> S "w"
invalidType :: Expr
invalidType = ("T", ui 0) :-> ("T1", ("t", S "T") :-> S "t") :-> S "T1"

someFunc :: IO ()
someFunc = do
    showLam "id" id'
    showLam "i:+1" $ ("i", L) :-> id' :@ "i" :+ 1
    showLam "higher" higher
    showLam "bigger" bigger
    showLam "r" $ ui 1
    showLam "test"  $ nf $ ("i", L) :-> ("r", us "i") :-> ("l", S "r") :-> id' :@ S "i" :@ S "r" :@ S "l"
    showLam "level" level
    showLam "false" false
    showLam "maxleveli" maxleveli
    showLam "maxlevelj" maxlevelj
    showLam "invalideLevel" invalideLevel
    showLam "invalidType" invalidType
    showLam "validErased" $ ("i", Erased L) :-> ("T", us "i") :-> ("t", S "T" ) :-> S "T" ::> S "t"
    showLam "invalidErased" $ ("i", Erased L) :-> ("T", us "i") :-> ("t", S "T" ) :->  L ::> S "i"
    print $ betaEq (("", S "t") :-> S "t") (("", S "t") :-> S "t")
    print $ typeCheckVar (("t", ui 1) :-> ("", S "t") :-> S "t") (("r", ui 1) :-> ("x", S "r") :-> S "x")

