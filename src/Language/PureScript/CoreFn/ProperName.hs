module Language.PureScript.CoreFn.ProperName where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Text (Text)
import GHC.Generics (Generic)

{- | Proper names, i.e. capitalized names
  for e.g. module names, type/data constructors.
-}
newtype ProperName (a ∷ ProperNameType) = ProperName {runProperName ∷ Text}
  deriving stock (Show, Eq, Ord, Generic)

instance ToJSON (ProperName a) where
  toJSON = toJSON . runProperName

instance FromJSON (ProperName a) where
  parseJSON = fmap ProperName . parseJSON

-- | The closed set of proper name types.
data ProperNameType
  = TypeName
  | ConstructorName
  | ClassName
  | Namespace

{- |
Coerces a ProperName from one ProperNameType to another. This should be used
with care, and is primarily used to convert ClassNames into TypeNames after
classes have been desugared.
-}
coerceProperName ∷ ProperName a → ProperName b
coerceProperName = ProperName . runProperName
