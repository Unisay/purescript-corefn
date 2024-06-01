{-# LANGUAGE TemplateHaskell #-}

-- | Defines the types of source code comments
module Language.PureScript.Comments where

import Data.Aeson.TH
  ( Options (..)
  , SumEncoding (..)
  , defaultOptions
  , deriveJSON
  )
import Data.Text (Text)
import GHC.Generics (Generic)

data Comment
  = LineComment Text
  | BlockComment Text
  deriving stock (Show, Eq, Ord, Generic)

$(deriveJSON (defaultOptions {sumEncoding = ObjectWithSingleField}) ''Comment)
