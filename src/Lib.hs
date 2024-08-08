module Lib (module Lib) where

import Data.List ( (\\), intercalate )
import Control.Monad (unless)
import Prelude hiding (id)
import Data.Either (fromRight)
import Data.Map.Strict (Map, singleton, insert, delete, toList, keys)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Bifunctor (second)

type Sym = String
type Levels = Map Sym Int
type Type = Expr
data Expr
    = L -- Type for unvirse level
    | Sym :+ Int -- Constructor of a level
    | U Levels -- Universe where the level is the max of each Sym+Int
    | (Sym, Type) :-> Expr -- Dependent lambda
    | Expr :@ Expr -- Application
    -- | (Sym, Expr) :/\ Expr -- Depedent intersection type
    -- | Expr :^ Expr -- Dependent intersection term
    -- | I1 Expr -- Term of the dependent intersection with first type
    -- | I2 Expr -- Term of the dependent intersection with second type
    | Type ::> Expr -- Left typing: Type ::> term
    | Erased Expr
    | E Sym Expr Expr
    | S Sym -- Symbol
    deriving (Eq, Read)
infixr 2 :->
infixl 7 :@
infix 6 ::>
infix 8 :+

instance Show Expr where
    show (S s) = s
    show (E s v e) = s ++ " = " ++ show v ++ "; " ++ show e
    show (Erased e) = "'" ++ show e
    show (f@((_, _) :-> _) :@ x) = "[" ++ show f ++ "] " ++ showApp x
    show (f :@ x) = show f ++ " " ++ showApp x
    show ((s, t) :-> e)
        | null s = show t ++ " -> " ++ show e
        | t == S "" = s ++ " -> " ++ show e
        | otherwise = "(" ++ s ++ ": " ++ show t ++ ") -> " ++ show e
    show (t ::> e) = show t ++ " :> " ++ show e
    show (U l) = "U " ++ showL l
    show (s :+ i) = s ++ "+" ++ show i
    show L = "L"

showApp :: Expr -> String
showApp (f :@ x) = "[" ++ show f ++ " " ++ showApp x ++ "]"
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

freeVars :: Expr -> [Sym]
freeVars (S s) = [s]
freeVars (Erased e) = freeVars e
freeVars (f :@ a) = freeVars f ++ freeVars a
freeVars (t ::> e) = freeVars t ++ freeVars e
freeVars ((s, t) :-> e) = freeVars t ++ (freeVars e \\ [s])
freeVars (U ls) = keys ls
freeVars (s :+ _) = [s]
freeVars (E s v e) = freeVars v ++ (freeVars e \\ [s])
freeVars L = []

-- whnf :: Expr -> Expr
-- whnf expr = spine expr []
--     where
--         spine (f :@ x) xs = spine f (x:xs)
--         spine (_ ::> e) xs = spine e xs
--         spine ((s, _t) :-> e) (x:xs) = spine (subst s x e) xs
--         spine f xs = foldl (:@) f xs

nf :: Expr -> Expr
nf expr = spine expr []
    where
        spine (f :@ x) xs = spine f (x:xs)
        spine (t ::> e) [] = nf t ::> nf e
        spine ((s, t) :-> e) [] = (s, nf t) :-> nf e
        spine ((s, _t) :-> e) (x:xs) = spine (subst s x e) xs
        spine (E s v e) xs = spine (subst s v e) xs
        spine (Erased e) [] = Erased (nf e)
        spine f xs = app f xs
        app f xs = foldl (:@) f (map nf xs)

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
        sub (E s' v e')
          | s == s' = E s' (sub v) e'
          | s' `elem` fsx =
            let s'' = newSym e' s'
                e'' = substVar s' s'' e'
            in E s'' (sub v) (sub e'')
          | otherwise = E s' (sub v) (sub e')
        sub ul@(U ls) = case x of
            S l -> case Map.lookup s ls of
                Just i -> U (insert l i $ delete s ls)
                Nothing -> ul
            s' :+ i' -> case Map.lookup s ls of
                Just i -> U (insert s' (i+i') $ delete s ls)
                Nothing -> ul
            _ -> ul
        sub l@(s' :+ i) = case x of
            S l' -> if s' == s then l' :+ i else l
            s'' :+ i' -> if s' == s then s'' :+ (i+i') else l
            _ -> l
        sub (Erased e) = Erased (sub e)
        sub L = L

        fsx = freeVars x
        newSym e' s' = loop s'
            where
                loop s'' = if s'' `elem` vars then loop (s' ++ "'") else s''
                vars = fsx ++ freeVars e'

substVar :: Sym -> Sym -> Expr -> Expr
substVar s s' = subst s (S s')

data Direction = Symmetric | Directional

alphaEq :: Direction -> Expr -> Expr -> Bool
alphaEq _ (S s) (S s') = s == s'
alphaEq d (f :@ x) (f' :@ x') = alphaEq d f f' && alphaEq d x x'
alphaEq d (t ::> e) (t' ::> e') = alphaEq d e e' && alphaEq d t t'
alphaEq d ((s, t) :-> e) ((s', t') :-> e') = alphaEq d e (substVar s' s e') && alphaEq d t t'
alphaEq Symmetric (U ls) (U ls') = ls == ls'
alphaEq Directional (U ls) (U ls') = universeValid ls ls'
alphaEq _ L L = True
alphaEq Symmetric (s :+ i) (s' :+ i') = s == s' && i == i'
alphaEq Directional (s :+ i) (s' :+ i') = s == s' && i >= i'
alphaEq d (E s v e) e' = alphaEq d (subst s v e) e'
alphaEq d e (E s v e') = alphaEq d e (subst s v e')
alphaEq d (Erased e) (Erased e') = alphaEq d e e'
alphaEq _ _ _ = False

-- first value correspond to the expected level, so the largest one in the case of cumulative universes
universeValid :: Levels -> Levels -> Bool
universeValid l1 l2 = fromMaybe False (req (toList l2))
    where
        req [] = Nothing
        req [l] = iner l
        req (l:ls) = iner l >>= \b -> (b||) <$> req ls
        iner (l, i) = (i<=) <$> Map.lookup l l1

betaEq :: Expr -> Expr -> Bool
betaEq e1 e2 = alphaEq Symmetric (nf e1) (nf e2)

-- In case it is used to check that the type of a term correspond to an expected type, the expeted type should be provided first and the term type second.
betaEqD :: Expr -> Expr -> Bool
betaEqD e1 e2 = alphaEq Directional (nf e1) (nf e2)

erased :: Expr -> Expr
erased (S s) = S s
erased (f :@ (Erased _)) = erased f
erased (f :@ x) = erased f :@ erased x
erased (_t ::> e) = erased e
erased ((_, Erased _) :-> e) = erased e
erased ((s, _t) :-> e) = (s, S "") :-> erased e
erased (E s v e) = erased $ subst s v e
erased (U ls) = U ls
erased (s :+ i) = s :+ i
erased (Erased e) = Erased e
erased L = L

erasedBetaEq :: Expr -> Expr -> Bool
erasedBetaEq e1 e2 = alphaEq Symmetric (erased . nf $ e1) (erased . nf $ e2)

erasedBetaEqD :: Expr -> Expr -> Bool
erasedBetaEqD e1 e2 = alphaEq Directional (erased . nf $ e1) (erased . nf $ e2)

newtype Env = Env [(Sym, Expr)] deriving (Show)

initialEnv :: Env
initialEnv = Env []

extend :: Env -> Sym -> Expr -> Env
extend (Env ls) s t = Env ((s, t) : ls)

type ErrorMsg = String
type TC a = Either ErrorMsg a
throwError :: String -> TC a
throwError = Left

findVar :: Env -> Sym -> TC (IsType, Expr)
findVar (Env ls) s =
    case lookup s ls of
        Just t -> return $ case t of
            (U l) -> (True, U l)
            v -> (False, v)
        Nothing -> throwError $ "Connat find variable " ++ s

type IsType = Bool
tCheck :: Env -> Expr -> TC (IsType, Expr)
tCheck env (S s) = findVar env s
tCheck env (Erased e) = second Erased <$> tCheck env e
tCheck env (f :@ x) = do
    (isT, tf) <- tCheck env f
    case tf of
        (s, t) :-> b -> do
            (_isT', tx) <- tCheck env x
            unless (betaEqD t tx) $ throwError "Bad function argument type"
            return (isT, subst s x b)
        e -> throwError $ "Non-function in application (" ++ show (f :@ x) ++ "): " ++ show e ++ "."
tCheck env (t ::> e) = do
    (isT, te) <- tCheck env e
    unless (betaEqD t te) $ throwError $ "Type missmatch (" ++ show (nf t) ++ "," ++ show (nf te) ++ ") in " ++ show (t ::> e) ++ "."
    return (isT, t)
tCheck env ((s, t) :-> e) = do
    (isT, tt) <- tCheck env t
    unless isT $ throwError $ "The type of a type must be a universe, which is not the case for \"" ++ s ++ ": " ++ show t ++ ": " ++ show tt ++ "\"."
    let env' = extend env s t
    (_isT', te) <- tCheck env' e
    return (True, (s, t) :-> te)
tCheck env (E s v e) = tCheck env v *> tCheck env (subst s v e)
tCheck _ (U ls) = return (True, U ((+1) <$> ls))
tCheck env (s :+ _i) = findVar env s
tCheck _ L = return (True, U (singleton "" 0))

isUniverse :: Expr -> Bool
isUniverse (Erased (U _)) = True
isUniverse (U _) = True
isUniverse _ = False

universe :: Env -> Expr -> TC Levels
universe _ L = return $ singleton "" 0
universe _ (s :+ i) = throwError $ "The term \"" ++ s ++ "+" ++ show i ++ ")\" does not have a universe."
universe _ (U ls) = return ((+1) <$> ls)
universe env (S s) = findVar env s >>= ((((+(-1)) <$>) <$>) . universe env) . snd
universe env (f :@ _) = universe env f
universe env (_ ::> e) = universe env e
universe env ((s, t) :-> e) = do
    u1 <- universe env t
    u2 <- universe (extend env s t) e
    return $ maxLevel u1 u2
universe env (E s v e) = universe env (subst s v e)
universe env (Erased e) = universe env e
-- universe _ (Erased e) = throwError $ "An erased value \"'e\" does not have a universe, for \"'e = " ++ show e ++ "\""

maxLevel :: Levels -> Levels -> Levels
maxLevel l1 l2 = lmax l1 (toList l2)
    where
        lmax acc [] = acc
        lmax acc ((l, i):ls) = lmax (Map.insertWith max l i acc) ls

typeCheck :: Expr -> TC Expr
typeCheck = (snd <$>) . tCheck initialEnv

showLam :: Sym -> Expr -> IO ()
showLam s lam = case typeCheck lam of
     Right t -> do
         putStrLn $ s ++ " :: " ++ show t ++ " | " ++ show (case fromRight (singleton "ERROR" (-1)) (universe initialEnv t) of ls -> U ls)
         putStrLn $ s ++ " = " ++ show (erased lam)
     Left err -> putStrLn $ s ++ ": " ++ show err

typeCheckVar :: Expr -> Expr -> TC Bool
typeCheckVar ty var = do
    tv <- typeCheck var
    return $ betaEqD ty tv

lv :: Sym -> Int -> Levels
lv = singleton

ui :: Int -> Expr
ui i = U (singleton "" i)
us :: Sym -> Expr
us l = U (singleton l 0)
u :: Sym -> Int -> Expr
u l i = U (singleton l i)

id :: Expr
id = ("i", L) :-> ("t", us "i") :-> ("x", S "t") :-> S "t" ::> S "x"
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

zero :: Expr
zero = ("a", ui 1) :-> ("b", ui 1) :-> ("s", S "a") :-> ("z", S "b") :-> S "z"

someFunc :: IO ()
someFunc = do
    showLam "zero" zero
    showLam "id" id
    showLam "i:+1" $ ("i", L) :-> id :@ "i" :+ 1
    showLam "higher" higher
    showLam "bigger" bigger
    showLam "r" $ ui 1
    showLam "test"  $ nf $ ("i", L) :-> ("r", us "i") :-> ("l", S "r") :-> id :@ S "i" :@ S "r" :@ S "l"
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

