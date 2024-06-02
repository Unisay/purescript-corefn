module Language.PureScript.CoreFn.ModuleName
  ( ModuleName
  , moduleNameToText
  , unsafeModuleNameFromText
  , moduleNameFromText
  , isBuiltinModuleName
  ) where

import Data.Aeson
  ( FromJSON (parseJSON)
  , FromJSONKey (fromJSONKey)
  , ToJSON (toJSON)
  , ToJSONKey (toJSONKey)
  , withArray
  )
import Data.Char qualified as Char
import Data.Functor.Contravariant (Contravariant (contramap))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import GHC.Generics (Generic)

newtype ModuleName = ModuleName Text
  deriving newtype (Eq, Ord)
  deriving stock (Show, Generic)

moduleNameToText ∷ ModuleName → Text
moduleNameToText (ModuleName mn) = mn

unsafeModuleNameFromText ∷ Text → ModuleName
unsafeModuleNameFromText = ModuleName

moduleNameFromText ∷ Text → Maybe ModuleName
moduleNameFromText mn =
  if Text.any (not . isModuleNameChar) mn
    then Nothing
    else Just (ModuleName mn)
 where
  isModuleNameChar ∷ Char → Bool
  isModuleNameChar c = Char.isAlphaNum c || c == '_' || c == '.'

isBuiltinModuleName ∷ ModuleName → Bool
isBuiltinModuleName (ModuleName mn) =
  mn == "Prim" || "Prim." `Text.isPrefixOf` mn

instance ToJSON ModuleName where
  toJSON name = toJSON (Text.splitOn "." (moduleNameToText name))

instance FromJSON ModuleName where
  parseJSON = withArray "ModuleName" $ \names → do
    names' ← traverse parseJSON names
    pure (ModuleName (Text.intercalate "." (Vector.toList names')))

instance ToJSONKey ModuleName where
  toJSONKey = contramap moduleNameToText toJSONKey

instance FromJSONKey ModuleName where
  fromJSONKey = unsafeModuleNameFromText <$> fromJSONKey
