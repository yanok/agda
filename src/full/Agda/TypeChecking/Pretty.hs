{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Agda.TypeChecking.Pretty where

import Control.Applicative hiding (empty)

import Agda.Syntax.Position
import Agda.Syntax.Common hiding (Arg, Dom, NamedArg, ArgInfo)
import qualified Agda.Syntax.Common as Common
import Agda.Syntax.Internal
import Agda.Syntax.Literal
import Agda.Syntax.Translation.InternalToAbstract
import Agda.Syntax.Translation.AbstractToConcrete
import qualified Agda.Syntax.Abstract as A
import qualified Agda.Syntax.Concrete as C
import qualified Agda.Syntax.Abstract.Pretty as P
import qualified Agda.Syntax.Concrete.Pretty as P

import Agda.TypeChecking.Monad

import qualified Agda.Utils.Pretty as P
import Agda.Utils.Permutation (Permutation)

#include "../undefined.h"
import Agda.Utils.Impossible

---------------------------------------------------------------------------
-- * Wrappers for pretty printing combinators
---------------------------------------------------------------------------

type Doc = P.Doc

empty, comma, colon, equals :: TCM Doc

empty      = return P.empty
comma      = return P.comma
colon      = return P.colon
equals     = return P.equals
pretty x   = return $ P.pretty x
prettyA x  = P.prettyA x
prettyAs x = P.prettyAs x
text :: String -> TCM Doc
text s     = return $ P.text s
pwords s   = map return $ P.pwords s
fwords s   = return $ P.fwords s
sep, fsep, hsep, vcat :: [TCM Doc] -> TCM Doc
sep ds     = P.sep <$> sequence ds
fsep ds    = P.fsep <$> sequence ds
hsep ds    = P.hsep <$> sequence ds
hcat ds    = P.hcat <$> sequence ds
vcat ds    = P.vcat <$> sequence ds
($$), ($+$), (<>), (<+>) :: TCM Doc -> TCM Doc -> TCM Doc
d1 $$ d2   = (P.$$) <$> d1 <*> d2
d1 $+$ d2  = (P.$+$) <$> d1 <*> d2
d1 <> d2   = (P.<>) <$> d1 <*> d2
d1 <+> d2  = (P.<+>) <$> d1 <*> d2
nest n d   = P.nest n <$> d
braces d   = P.braces <$> d
dbraces d  = P.dbraces <$> d
brackets d = P.brackets <$> d
parens d   = P.parens <$> d

prettyList ds = brackets $ fsep $ punctuate comma ds

punctuate _ [] = []
punctuate d ds = zipWith (<>) ds (replicate n d ++ [empty])
    where
        n = length ds - 1

---------------------------------------------------------------------------
-- * The PrettyTCM class
---------------------------------------------------------------------------

class PrettyTCM a where
    prettyTCM :: a -> TCM Doc

instance PrettyTCM a => PrettyTCM (Closure a) where
    prettyTCM cl = enterClosure cl prettyTCM

instance PrettyTCM a => PrettyTCM [a] where
  prettyTCM = prettyList . map prettyTCM

instance (PrettyTCM a, PrettyTCM b) => PrettyTCM (a,b) where
  prettyTCM (a, b) = parens $ prettyTCM a <> comma <> prettyTCM b

instance PrettyTCM Nat where prettyTCM = text . show
instance PrettyTCM Bool where prettyTCM = text . show
instance PrettyTCM Term where prettyTCM x = prettyA =<< reify x
instance PrettyTCM Type where prettyTCM x = prettyA =<< reify x
instance PrettyTCM Sort where prettyTCM x = prettyA =<< reify x
instance PrettyTCM DisplayTerm where prettyTCM x = prettyA =<< reify x
instance PrettyTCM NamedClause where prettyTCM x = prettyA =<< reify x
instance PrettyTCM Level where prettyTCM x = prettyA =<< reify (Level x)
instance PrettyTCM Permutation where prettyTCM = text . show

instance PrettyTCM Position where
  prettyTCM p = do
    text $ show p

instance PrettyTCM Interval where
  prettyTCM i = do
    text $ show i

instance PrettyTCM Range where
  prettyTCM r = do
    text $ show r

instance PrettyTCM ClauseBody where
  prettyTCM b = do
    (binds, body) <- walk b
    sep [ brackets (fsep binds), return body ]
    where
      walk NoBody = return ([], P.text "()")
      walk (Body v) = (,) [] <$> prettyTCM v
      walk (Bind b) = do
        (bs, v) <- underAbstraction_ b walk
        return (text (argNameToString $ absName b) : bs, v)

instance (PrettyTCM a, PrettyTCM b) => PrettyTCM (Judgement a b) where
  prettyTCM (HasType a t) = prettyTCM a <+> text ":" <+> prettyTCM t
  prettyTCM (IsSort  a t) = text "Sort" <+> prettyTCM a <+> text ":" <+> prettyTCM t

instance PrettyTCM MetaId where
  prettyTCM x = do
   mn <- getMetaNameSuggestion x
   text $ show (NamedMeta mn x)

instance PrettyTCM a => PrettyTCM (Blocked a) where
  prettyTCM (Blocked x a) = text "[" <+> prettyTCM a <+> text "]" <> text (show x)
  prettyTCM (NotBlocked x) = prettyTCM x

instance (Reify a e, ToConcrete e c, P.Pretty c) => PrettyTCM (Named_ a) where
    prettyTCM x = prettyA =<< reify x

instance (Reify a e, ToConcrete e c, P.Pretty c) => PrettyTCM (Arg a) where
    prettyTCM x = prettyA =<< reify x

instance (Reify a e, ToConcrete e c, P.Pretty c) => PrettyTCM (Dom a) where
    prettyTCM x = prettyA =<< reify x

-- instance (Reify a e, ToConcrete e c, P.Pretty c, PrettyTCM a) => PrettyTCM (Elim' a) where
instance PrettyTCM Elim where
  prettyTCM (Apply v) = text "$" <+> prettyTCM v
  prettyTCM (Proj f)  = text "." <> prettyTCM f

instance PrettyTCM a => PrettyTCM (MaybeReduced a) where
  prettyTCM = prettyTCM . ignoreReduced

instance PrettyTCM A.Expr where
    prettyTCM = prettyA

instance PrettyTCM C.Name where
  prettyTCM = text . show

instance PrettyTCM Relevance where
  prettyTCM Irrelevant = text "."
  prettyTCM NonStrict  = text ".."
  prettyTCM Relevant   = empty
  prettyTCM Forced     = empty
  prettyTCM UnusedArg  = empty

instance PrettyTCM Comparison where
  prettyTCM CmpEq  = text "=="
  prettyTCM CmpLeq = text "=<"

instance PrettyTCM ProblemConstraint where
  prettyTCM (PConstr pid c) = brackets (text $ show pid) <+> prettyTCM c

instance PrettyTCM Constraint where
    prettyTCM c = case c of
        ValueCmp cmp ty s t ->
            sep [ sep [ prettyTCM s
                      , prettyTCM cmp <+> prettyTCM t
                      ]
                , nest 2 $ text ":" <+> prettyTCM ty
                ]
        ElimCmp cmps t v us vs ->
          sep [ sep [ prettyTCM us
                    , nest 2 $ text "~~" <+> prettyTCM vs
                    ]
              , text ":" <+> prettyTCM t ]
        LevelCmp cmp a b ->
            sep [ prettyTCM a
                , prettyTCM cmp <+> prettyTCM b
                ]
        TypeCmp cmp a b ->
            sep [ prettyTCM a
                , prettyTCM cmp <+> prettyTCM b
                ]
        TelCmp a b cmp tela telb ->
            sep [ prettyTCM tela
                , prettyTCM cmp <+> prettyTCM telb
                ]
        SortCmp cmp s1 s2 ->
            sep [ prettyTCM s1
                , prettyTCM cmp <+> prettyTCM s2
                ]
        Guarded c pid ->
            sep [ prettyTCM c
                , nest 2 $ brackets $ text "blocked on problem" <+> text (show pid)
                ]
        UnBlock m   -> do
            -- BlockedConst t <- mvInstantiation <$> lookupMeta m
            mi <- mvInstantiation <$> lookupMeta m
            case mi of
              BlockedConst t ->
                sep [ text (show m) <+> text ":="
                    , nest 2 $ prettyTCM t
                    ]
              PostponedTypeCheckingProblem cl _ -> enterClosure cl $ \p ->
                sep [ text (show m) <+> text ":="
                    , nest 2 $ prettyTCM p ]
              Open{}  -> __IMPOSSIBLE__
              OpenIFS{}  -> __IMPOSSIBLE__
              InstS{} -> __IMPOSSIBLE__
              InstV{} -> __IMPOSSIBLE__
        FindInScope m Nothing -> do
            t <- getMetaType m
            sep [ text $ "Find in scope " ++ (show m) ++ " :" ++ (show t) ++ " (no candidate for now)"
                ]
        FindInScope m (Just cands) -> do
            t <- getMetaType m
            sep [ text $ "Find in scope " ++ (show m) ++ " :"
                , nest 2 $ prettyTCM t
                , sep $ flip map cands $ \(t,ty) ->
                           prettyTCM t <+> text ": " <+> prettyTCM ty
                ]
        IsEmpty r t ->
            sep [ text "Is empty:", nest 2 $ prettyTCM t ]

instance PrettyTCM TypeCheckingProblem where
  prettyTCM (CheckExpr e a) =
    sep [ prettyA e <+> text ":?", prettyTCM a ]
  prettyTCM (CheckArgs _ _ _ es t0 t1 _) =
    sep [ parens $ text "_ :" <+> prettyTCM t0
        , nest 2 $ prettyList $ map prettyA es
        , nest 2 $ text ":?" <+> prettyTCM t1 ]

instance PrettyTCM Literal where
  prettyTCM = text . show

instance PrettyTCM Name where
    prettyTCM x = P.pretty <$> abstractToConcrete_ x

instance PrettyTCM QName where
    prettyTCM x = P.pretty <$> abstractToConcrete_ x

instance PrettyTCM ModuleName where
  prettyTCM x = P.pretty <$> abstractToConcrete_ x

instance PrettyTCM ConHead where
  prettyTCM = prettyTCM . conName

instance PrettyTCM Telescope where
  prettyTCM tel = P.fsep . map P.pretty <$> (do
      tel <- reify tel
      runAbsToCon $ bindToConcrete tel (return . concat)
    )

newtype PrettyContext = PrettyContext Context

instance PrettyTCM PrettyContext where
  prettyTCM (PrettyContext ctx) = P.fsep . reverse <$> pr (map ctxEntry ctx)
      where
          pr :: [Dom (Name, Type)] -> TCM [P.Doc]
          pr []            = return []
          pr (Common.Dom info (x,t) : ctx) = escapeContext 1 $ do
              d    <- prettyTCM t
              x    <- prettyTCM x
              dctx <- pr ctx
              -- TODO guilhem: show colors
              return $ P.pRelevance info' (par (P.hsep [ x, P.text ":", d])) : dctx
            where
              info' = mapArgInfoColors (const []) info
              par = case argInfoHiding info of
                     NotHidden -> P.parens
                     Hidden    -> P.braces
                     Instance  -> P.dbraces

instance PrettyTCM Context where
  prettyTCM = prettyTCM . PrettyContext

instance PrettyTCM Pattern where
  prettyTCM = showPat
    where
      showPat (VarP x)                  = text $ patVarNameToString x
      showPat (DotP t)                  = text $ ".(" ++ show t ++ ")"
      showPat (ConP c Nothing ps)       = parens $
        prettyTCM c <+> fsep (map (showPat . namedArg) ps)
      showPat (ConP c (Just (b, t)) ps) = (if b then braces else parens) $
        prettyTCM c <+> fsep (map (showPat . namedArg) ps) <+> text ":" <+> prettyTCM t
      showPat (LitP l)                  = text (show l)
      showPat (ProjP q)                 = text (show q)

instance PrettyTCM RewriteRule where
  prettyTCM (RewriteRule q gamma lhs rhs b) = inTopContext $ do
    prettyTCM q     <+> text " rule " <+> do
    prettyTCM gamma <+> text " |- "   <+> do
    addContext gamma $ do
    prettyTCM lhs   <+> text " --> "  <+> do
    prettyTCM rhs   <+> text " : "    <+> do
    prettyTCM b
