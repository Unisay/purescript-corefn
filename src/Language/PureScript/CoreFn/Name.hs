{-# LANGUAGE DataKinds #-}

module Language.PureScript.CoreFn.Name where

import GHC.Generics (Generic)
import Language.PureScript.CoreFn.Ident (Ident)
import Language.PureScript.CoreFn.ModuleName (ModuleName)
import Language.PureScript.CoreFn.ProperName
  ( ProperName
  , ProperNameType (ClassName, ConstructorName, TypeName)
  )

-- | A sum of the possible name types, useful for error and lint messages.
data Name
  = IdentName Ident
  | TyName (ProperName 'TypeName)
  | DctorName (ProperName 'ConstructorName)
  | TyClassName (ProperName 'ClassName)
  | ModName ModuleName
  deriving stock (Eq, Ord, Show, Generic)

getIdentName ∷ Name → Maybe Ident
getIdentName (IdentName name) = Just name
getIdentName _ = Nothing

getTypeName ∷ Name → Maybe (ProperName 'TypeName)
getTypeName (TyName name) = Just name
getTypeName _ = Nothing

getDctorName ∷ Name → Maybe (ProperName 'ConstructorName)
getDctorName (DctorName name) = Just name
getDctorName _ = Nothing

getClassName ∷ Name → Maybe (ProperName 'ClassName)
getClassName (TyClassName name) = Just name
getClassName _ = Nothing
