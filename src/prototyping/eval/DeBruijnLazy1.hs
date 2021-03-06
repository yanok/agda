{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}

-- parent: DeBruijnCBN7

-- Lazy evaluation

module DeBruijnLazy1 where

import Prelude hiding (mapM)

import Control.Applicative
import Data.Traversable
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Map (Map, (!))
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Syntax as S
import Syntax ( AppView(..), Pat(..), appView )
import Utils
import Pretty
import Pointer

data ExpR = Var Int [Exp]
          | Con Name [Exp]
          | Def Name [Exp]
          | Lam Exp [Exp]

type Exp = Ptr ExpR

data Case
        = Done Exp
        | Skip Case
        | Bind Case
        | Split (NameMap Case)

type Name = Int
type NameMap = Map Name

type M = HeapM ExpR

appsR :: ExpR -> [Exp] -> ExpR
appsR e         []  = e
appsR (Var n es) es' = Var n $ es ++ es'
appsR (Con c es) es' = Con c $ es ++ es'
appsR (Def c es) es' = Def c $ es ++ es'
appsR (Lam e es) es' = Lam e $ es ++ es'

apps :: Exp -> [Exp] -> M Exp
apps e es = do
    v <- deref e
    alloc (appsR v es)

var n es = alloc (Var n es)
con c es = alloc (Con c es)
def c es = alloc (Def c es)
lam e es = alloc (Lam e es)

type Sig = NameMap Case

class Names a where
    getNames :: a -> Set S.Name

instance Names S.Exp where
    getNames e = case e of
        S.Var _   -> Set.empty
        S.Def c   -> Set.singleton c
        S.Con c   -> Set.singleton c
        S.Lam e   -> getNames e
        S.App u v -> Set.union (getNames u) (getNames v)

instance Names S.Pat where
    getNames p = case p of
        VarP      -> Set.empty
        WildP     -> Set.empty
        ConP c ps -> Set.insert c $ getNames ps

instance Names S.Clause where
    getNames (S.Clause ps v) = getNames (ps, v)

instance Names a => Names (Map k a) where
    getNames = getNames . Map.elems

instance Names a => Names [a] where
    getNames = Set.unions . map getNames

instance (Names a, Names b) => Names (a, b) where
    getNames (x, y) = Set.union (getNames x) (getNames y)

class Compile a c | a -> c where
    compile :: a -> c

instance Compile S.Sig (M (Sig, NameMap S.Name, Map S.Name Name)) where
    compile sig = do
        sig' <- Map.fromList <$> mapM comp (Map.toList sig)
        return (sig', nameMap, idMap)
        where
            ns      = Set.toList $ Set.union (getNames sig) (Set.fromList $ Map.keys sig)
            nameMap = Map.fromList $ zip [0..] ns
            idMap   = Map.fromList $ zip ns [0..]

            comp (c, cl) = do
                cl' <- compile cl idMap
                return (idMap ! c, cl')

instance Compile S.Exp (Map S.Name Name -> M Exp) where
    compile e nmap = case appView e of
        Apps (S.Var n) es -> var n                =<< comps es
        Apps (S.Con c) es -> con (nmap ! c)       =<< comps es
        Apps (S.Def c) es -> def (nmap ! c)       =<< comps es
        Apps (S.Lam v) es -> do
            v' <- compile v nmap
            vs <- comps es
            lam v' vs
        where
            comps es = mapM (flip compile nmap) es

instance Compile [S.Clause] (Map S.Name Name -> M Case) where
    compile cs nmap = do
        np <- nextPatterns cs
        case np of
            Right [v]   -> return $ Done v
            Right []    -> error $ "no rhs: " ++ show cs
            Right (_:_) -> error $ "overlapping patterns: " ++ show cs
            Left pcs    -> case conOrVar pcs of
                Left cs   -> Bind <$> compile cs nmap
                Right ccs -> Split <$> mapM (flip compile nmap) (Map.fromList ccs)
        where
            patterns (S.Clause ps     _) = ps
            body     (S.Clause _      v) = compile v nmap
            next     (S.Clause (p:ps) v) = (p, S.Clause ps v)

            nextPatterns :: [S.Clause] -> M (Either [(Pat, S.Clause)] [Exp])
            nextPatterns cs
                | all null pss  = Right <$> mapM body cs
                | otherwise     = return $ Left $ map next cs
                where
                    pss = map patterns cs

            conOrVar :: [(Pat, S.Clause)] -> Either [S.Clause] [(Name, [S.Clause])]
            conOrVar cs
                | all (isVar . fst) cs = Left $ map snd cs
                | all (isCon . fst) cs = Right $
                    map splitCon
                    $ groupBy ((==) `on` conName `on` fst)
                    $ sortBy (compare `on` conName `on` fst)
                    $ cs
                | otherwise            = error $ "bad clauses: " ++ show cs
                where
                    splitCon :: [(Pat, S.Clause)] -> (Name, [S.Clause])
                    splitCon cs = ( nmap ! conName (fst $ head cs)
                                  , map amendClause cs
                                  )
                        where
                            amendClause (ConP _ ps, S.Clause qs v) = S.Clause (ps ++ qs) v

                    isVar VarP  = True
                    isVar WildP = True
                    isVar _     = False

                    isCon (ConP _ _) = True
                    isCon _          = False

                    conName (ConP c _ ) = c
                    conArgs (ConP c vs) = vs

decompile :: NameMap S.Name -> Exp -> M S.Exp
decompile nmap e = do
    e <- deref e
    case e of
        Var n es -> S.apps (S.Var n) <$> mapM dec es
        Con c es -> S.apps (S.Con (nmap ! c)) <$> mapM dec es
        Def c es -> S.apps (S.Def (nmap ! c)) <$> mapM dec es
        Lam e es -> do
            e <- dec e
            es <- mapM dec es
            return $ S.Lam e `S.apps` es
    where
        dec = decompile nmap
        (!) = (Map.!)

-- Evaluation

raiseFrom :: Int -> Int -> Exp -> M Exp
raiseFrom n k e = do
    e <- deref e
    case e of
        Var m es
            | m < n     -> var m =<< mapM (raiseFrom n k) es
            | otherwise -> var (m + k) =<< mapM (raiseFrom n k) es
        Con c es        -> con c =<< mapM (raiseFrom n k) es
        Def c es        -> def c =<< mapM (raiseFrom n k) es
        Lam e es        -> do
            e  <- raiseFrom (n + 1) k e
            es <- mapM (raiseFrom n k) es
            lam e es

raise :: Int -> Exp -> M Exp
raise = raiseFrom 0

subst :: [Exp] -> Exp -> M Exp
subst us v = do
    v <- deref v
    case v of
        Var m es -> apps (us !! m) =<< mapM (subst us) es
        Con c es -> con c =<< mapM (subst us) es
        Def c es -> def c =<< mapM (subst us) es
        Lam t es -> do
            vz <- var 0 []
            us' <- mapM (raise 1) us
            e  <- subst (vz : us') t
            es <- mapM (subst us) es
            lam e es

matchDef :: Int -> Sig -> Case -> [Exp] -> M (Maybe Exp)
matchDef ctx sig c vs = match c [] vs
    where
        match (Done v)  sub vs     = Just <$> (flip apps vs =<< subst sub v)
        match _         sub []     = return Nothing
        match (Skip c)  sub (v:vs) = match c sub vs
        match (Bind c)  sub (v:vs) = match c (v : sub) vs
        match (Split m) sub (v:vs) = do
            v' <- whnf ctx sig v
            case v' of
                Con c ws -> case Map.lookup c m of
                    Just c' -> match c' sub (ws ++ vs)
                    Nothing -> return Nothing
                _ -> return Nothing

iota :: Int -> Sig -> Name -> [Exp] -> M ExpR
iota ctx sig c vs = case {-# SCC "lookupDef" #-} Map.lookup c sig of
    Nothing -> return $ Con c vs
    Just cs -> do
        mv <- matchDef ctx sig cs vs
        case mv of
            Nothing -> return $ Con c vs
            Just v  -> whnf ctx sig v

top :: Int -> Exp -> M [Exp]
top n v = (v :) <$> mapM (flip var []) [0..n - 1]

whnf :: Int -> Sig -> Exp -> M ExpR
whnf ctx sig v = onThunk v $ \v -> case v of
    Var n vs       -> return $ Var n vs
    Con c vs       -> return $ Con c vs
    Def c vs       -> iota ctx sig c vs
    Lam u (v : vs) -> do
        sub <- top ctx v
        whnf ctx sig =<< flip apps vs =<< subst sub u
    Lam u []       -> return $ Lam u []

eval' :: Int -> Sig -> Exp -> M ()
eval' ctx sig v = do
    whnf ctx sig v
    onThunk v $ \v -> case v of
        Lam u [] -> eval' (ctx + 1) sig u >> return v
        Var n vs -> mapM (eval' ctx sig) vs >> return v
        Con c vs -> mapM (eval' ctx sig) vs >> return v
        Def c vs -> mapM (eval' ctx sig) vs >> return v
    return ()

eval :: S.Sig -> S.Exp -> S.Exp
eval sig e = runHeap $ do
    (sig', nmap, imap) <- compile sig
    v <- compile e imap
    eval' 0 sig' v
    decompile nmap v
