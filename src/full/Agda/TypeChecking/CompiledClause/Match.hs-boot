
module Agda.TypeChecking.CompiledClause.Match where

-- import Agda.Syntax.Common
import Agda.Syntax.Internal
import Agda.TypeChecking.Monad.Base
import Agda.TypeChecking.Reduce.Monad
import Agda.TypeChecking.CompiledClause

matchCompiled :: CompiledClauses -> MaybeReducedArgs -> ReduceM (Reduced (Blocked Args) Term)
matchCompiledE :: CompiledClauses -> MaybeReducedElims -> ReduceM (Reduced (Blocked [Elim]) Term)
