module Language.PureScript.CoreFn.Module where

import Data.Map (Map)
import Language.PureScript.CoreFn.Comment (Comment)
import Language.PureScript.CoreFn.Expr (Bind)
import Language.PureScript.CoreFn.Ident (Ident)
import Language.PureScript.CoreFn.ModuleName (ModuleName)

-- | The CoreFn module representation
data Module a = Module
  { moduleName ∷ ModuleName
  , moduleComments ∷ [Comment]
  , modulePath ∷ FilePath
  , moduleImports ∷ [(a, ModuleName)]
  , moduleExports ∷ [Ident]
  , moduleReExports ∷ Map ModuleName [Ident]
  , moduleForeign ∷ [Ident]
  , moduleBindings ∷ [Bind a]
  }
  deriving stock (Functor, Show)
