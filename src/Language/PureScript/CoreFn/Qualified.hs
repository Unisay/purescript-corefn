module Language.PureScript.CoreFn.Qualified where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), ToJSON (..), toJSON2)
import Data.Aeson.Types (parseJSON2)
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.PureScript.CoreFn.ModuleName (ModuleName, moduleNameToText)
import Language.PureScript.CoreFn.SourcePos (SourcePos (..))

data QualifiedBy
  = BySourcePos SourcePos
  | ByModuleName ModuleName
  deriving stock (Show, Eq, Ord, Generic)

pattern ByNullSourcePos ∷ QualifiedBy
pattern ByNullSourcePos = BySourcePos (SourcePos 0 0)

isBySourcePos ∷ QualifiedBy → Bool
isBySourcePos (BySourcePos _) = True
isBySourcePos _ = False

byMaybeModuleName ∷ Maybe ModuleName → QualifiedBy
byMaybeModuleName = maybe ByNullSourcePos ByModuleName

toMaybeModuleName ∷ QualifiedBy → Maybe ModuleName
toMaybeModuleName (ByModuleName mn) = Just mn
toMaybeModuleName (BySourcePos _) = Nothing

-- | A qualified name, i.e. a name with an optional module name
data Qualified a = Qualified QualifiedBy a
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

showQualified ∷ (a → Text) → Qualified a → Text
showQualified f = \case
  Qualified (BySourcePos _) a → f a
  Qualified (ByModuleName name) a → moduleNameToText name <> "." <> f a

qualifiedModuleName ∷ Qualified a → Maybe ModuleName
qualifiedModuleName (Qualified qb _) = toMaybeModuleName qb

-- | Provide a default module name, if a name is unqualified
qualify ∷ ModuleName → Qualified a → (ModuleName, a)
qualify m (Qualified (BySourcePos _) a) = (m, a)
qualify _ (Qualified (ByModuleName m) a) = (m, a)

-- | Makes a qualified value from a name and module name.
mkQualified ∷ a → ModuleName → Qualified a
mkQualified name mn = Qualified (ByModuleName mn) name

-- | Remove the module name from a qualified name
disqualify ∷ Qualified a → a
disqualify (Qualified _ a) = a

{- |
Remove the qualification from a value when it is qualified with a particular
module name.
-}
disqualifyFor ∷ Maybe ModuleName → Qualified a → Maybe a
disqualifyFor mn (Qualified qb a) | mn == toMaybeModuleName qb = Just a
disqualifyFor _ _ = Nothing

{- |
Checks whether a qualified value is actually qualified with a module reference
-}
isQualified ∷ Qualified a → Bool
isQualified (Qualified (BySourcePos _) _) = False
isQualified _ = True

{- |
Checks whether a qualified value is not actually qualified
with a module reference
-}
isUnqualified ∷ Qualified a → Bool
isUnqualified = not . isQualified

-- | Checks whether a qualified value is qualified with a particular module
isQualifiedWith ∷ ModuleName → Qualified a → Bool
isQualifiedWith mn (Qualified (ByModuleName mn') _) = mn == mn'
isQualifiedWith _ _ = False

instance ToJSON a ⇒ ToJSON (Qualified a) where
  toJSON (Qualified qb a) = case qb of
    ByModuleName mn → toJSON2 (mn, a)
    BySourcePos ss → toJSON2 (ss, a)

instance FromJSON a ⇒ FromJSON (Qualified a) where
  parseJSON v = byModule <|> bySourcePos <|> byMaybeModuleName'
   where
    byModule = do
      (mn, a) ← parseJSON2 v
      pure $ Qualified (ByModuleName mn) a
    bySourcePos = do
      (ss, a) ← parseJSON2 v
      pure $ Qualified (BySourcePos ss) a
    byMaybeModuleName' = do
      (mn, a) ← parseJSON2 v
      pure $ Qualified (byMaybeModuleName mn) a
