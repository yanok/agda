{-# OPTIONS_GHC -fwarn-missing-signatures #-}

{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

{-| An info object contains additional information about a piece of abstract
    syntax that isn't part of the actual syntax. For instance, it might contain
    the source code position of an expression or the concrete syntax that
    an internal expression originates from.
-}

module Agda.Syntax.Info where

import Data.Typeable (Typeable)

import qualified Agda.Syntax.Concrete.Name as C
import Agda.Syntax.Common
import Agda.Syntax.Position
import Agda.Syntax.Concrete
import Agda.Syntax.Fixity
import Agda.Syntax.Scope.Base (ScopeInfo, emptyScopeInfo)

{--------------------------------------------------------------------------
    Meta information
 --------------------------------------------------------------------------}

data MetaInfo = MetaInfo
  { metaRange          :: Range
  , metaScope          :: ScopeInfo
  , metaNumber         :: Maybe Nat  -- ^ The 'MetaId', not the 'InteractionId'.
  , metaNameSuggestion :: String
  }
  deriving (Typeable, Show)

emptyMetaInfo :: MetaInfo
emptyMetaInfo = MetaInfo
  { metaRange          = noRange
  , metaScope          = emptyScopeInfo
  , metaNumber         = Nothing
  , metaNameSuggestion = ""
  }

instance HasRange MetaInfo where
  getRange = metaRange

instance KillRange MetaInfo where
  killRange m = m { metaRange = noRange }

{--------------------------------------------------------------------------
    General expression information
 --------------------------------------------------------------------------}

newtype ExprInfo = ExprRange Range
  deriving (Typeable, Show)

exprNoRange :: ExprInfo
exprNoRange = ExprRange noRange

instance HasRange ExprInfo where
  getRange (ExprRange r) = r

instance KillRange ExprInfo where
  killRange (ExprRange r) = exprNoRange

{--------------------------------------------------------------------------
    Module information
 --------------------------------------------------------------------------}

data ModuleInfo = ModuleInfo
  { minfoRange    :: Range
  , minfoAsTo     :: Range
    -- ^ The range of the \"as\" and \"to\" keywords,
    -- if any. Retained for highlighting purposes.
  , minfoAsName   :: Maybe C.Name
    -- ^ The \"as\" module name, if any. Retained for highlighting purposes.
  , minfoOpenShort :: Maybe OpenShortHand
  , minfoDirective :: Maybe ImportDirective
    -- ^ Retained for @abstractToConcrete@ of 'ModuleMacro'.
  }
  deriving (Typeable)

deriving instance (Show OpenShortHand, Show ImportDirective) => Show ModuleInfo

instance HasRange ModuleInfo where
  getRange = minfoRange

instance SetRange ModuleInfo where
  setRange r i = i { minfoRange = r }

instance KillRange ModuleInfo where
  killRange m = m { minfoRange = noRange }

---------------------------------------------------------------------------
-- Let info
---------------------------------------------------------------------------

newtype LetInfo = LetRange Range
  deriving (Typeable, Show)

instance HasRange LetInfo where
  getRange (LetRange r)   = r

instance KillRange LetInfo where
  killRange (LetRange r) = LetRange noRange

{--------------------------------------------------------------------------
    Definition information (declarations that actually define something)
 --------------------------------------------------------------------------}

data DefInfo = DefInfo
  { defFixity   :: Fixity'
  , defAccess   :: Access
  , defAbstract :: IsAbstract
  , defInstance :: IsInstance
  , defInfo     :: DeclInfo
  }
  deriving (Typeable, Show)

mkDefInfo :: Name -> Fixity' -> Access -> IsAbstract -> Range -> DefInfo
mkDefInfo x f a ab r = DefInfo f a ab NotInstanceDef (DeclInfo x r)

-- | Same as @mkDefInfo@ but where we can also give the @IsInstance@
mkDefInfoInstance :: Name -> Fixity' -> Access -> IsAbstract -> IsInstance -> Range -> DefInfo
mkDefInfoInstance x f a ab i r = DefInfo f a ab i (DeclInfo x r)

instance HasRange DefInfo where
  getRange = getRange . defInfo

instance SetRange DefInfo where
  setRange r i = i { defInfo = setRange r (defInfo i) }

instance KillRange DefInfo where
  killRange i = i { defInfo = killRange $ defInfo i }

{--------------------------------------------------------------------------
    General declaration information
 --------------------------------------------------------------------------}

data DeclInfo = DeclInfo
  { declName  :: Name
  , declRange :: Range
  }
  deriving (Typeable, Show)

instance HasRange DeclInfo where
  getRange = declRange

instance SetRange DeclInfo where
  setRange r i = i { declRange = r }

instance KillRange DeclInfo where
  killRange i = i { declRange = noRange }

{--------------------------------------------------------------------------
    Mutual block information
 --------------------------------------------------------------------------}

data MutualInfo = MutualInfo
  { mutualTermCheck :: TerminationCheck Name
  , mutualRange     :: Range
  }
  deriving (Typeable, Show)

instance HasRange MutualInfo where
  getRange = mutualRange

instance KillRange MutualInfo where
  killRange i = i { mutualRange = noRange }

{--------------------------------------------------------------------------
    Left hand side information
 --------------------------------------------------------------------------}

newtype LHSInfo = LHSRange Range
  deriving (Typeable, Show)

instance HasRange LHSInfo where
  getRange (LHSRange r) = r

instance KillRange LHSInfo where
  killRange (LHSRange r) = LHSRange noRange

{--------------------------------------------------------------------------
    Pattern information
 --------------------------------------------------------------------------}

-- | For a general pattern we can either remember just the source code
--   position or the entire concrete pattern it came from.
data PatInfo
  = PatRange Range
  | PatSource Range (Precedence -> Pattern)
      -- ^ Even if we store the original pattern we have to know
      --   whether to put parenthesis around it.
  deriving (Typeable)

instance Show PatInfo where
  show (PatRange r) = "PatRange " ++ show r
  show (PatSource r _) = "PatSource " ++ show r

instance HasRange PatInfo where
  getRange (PatRange r)    = r
  getRange (PatSource r _) = r

instance KillRange PatInfo where
  killRange (PatRange r)    = PatRange noRange
  killRange (PatSource r f) = PatSource noRange f

-- | Empty range for patterns.
patNoRange :: PatInfo
patNoRange = PatRange noRange

-- | Constructor pattern info.
data ConPatInfo = ConPatInfo
  { patImplicit :: Bool
    -- ^ Does this pattern come form the eta-expansion of an implicit pattern?
  , patInfo     :: PatInfo
  }

instance Show ConPatInfo where
  show (ConPatInfo b i) = (if b then ("implicit " ++) else id) $ show i

instance HasRange ConPatInfo where
  getRange = getRange . patInfo

instance KillRange ConPatInfo where
  killRange (ConPatInfo b i) = ConPatInfo b $ killRange i

instance SetRange ConPatInfo where
  setRange r (ConPatInfo b i) = ConPatInfo b $ PatRange r
