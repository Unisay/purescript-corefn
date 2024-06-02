{-# LANGUAGE TemplateHaskell #-}

module Language.PureScript.CoreFn.Ident where

import Data.Aeson
  ( SumEncoding (ObjectWithSingleField)
  , defaultOptions
  , sumEncoding
  )
import Data.Aeson.TH (deriveJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

{- |
This type is meant to be extended with any new uses for idents that come
along. Adding constructors to this type is cheaper than adding them to
`Ident` because functions that match on `Ident` can ignore all
`InternalIdent`s with a single pattern, and thus don't have to change if
a new `InternalIdentData` constructor is created.
-}
data InternalIdentData
  = -- Used by CoreFn.Laziness
    RuntimeLazyFactory
  | Lazy !Text
  deriving stock (Show, Eq, Ord, Generic)

-- | Names for value identifiers
data Ident
  = -- |
    -- An alphanumeric identifier
    Ident Text
  | -- |
    -- A generated name for an identifier
    GenIdent (Maybe Text) Integer
  | -- |
    -- A generated name used only for type-checking
    UnusedIdent
  | -- |
    -- A generated name used only for internal transformations
    InternalIdent !InternalIdentData
  deriving stock (Show, Eq, Ord, Generic)

$( deriveJSON
    (defaultOptions {sumEncoding = ObjectWithSingleField})
    ''InternalIdentData
 )

$( deriveJSON
    (defaultOptions {sumEncoding = ObjectWithSingleField})
    ''Ident
 )
