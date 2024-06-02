module Language.PureScript.CoreFn.SourcePos where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)

-- | Source position information
data SourcePos = SourcePos {sourcePosLine ∷ Int, sourcePosColumn ∷ Int}
  deriving stock (Show, Eq, Ord, Generic)

displaySourcePos ∷ SourcePos → Text
displaySourcePos sp =
  "line "
    <> Text.pack (show (sourcePosLine sp))
    <> ", column "
    <> Text.pack (show (sourcePosColumn sp))

displaySourcePosShort ∷ SourcePos → Text
displaySourcePosShort sp =
  Text.pack (show (sourcePosLine sp))
    <> ":"
    <> Text.pack (show (sourcePosColumn sp))

instance ToJSON SourcePos where
  toJSON SourcePos {..} =
    toJSON [sourcePosLine, sourcePosColumn]

instance FromJSON SourcePos where
  parseJSON arr = do
    [line, col] ← parseJSON arr
    pure $ SourcePos line col
