{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.PureScript.CoreFn.Reader where

import Control.Monad (unless)
import Control.Monad.Except (ExceptT)
import Control.Monad.Oops (CouldBe, CouldBeAnyOf, Variant, throw)
import Control.Monad.Oops qualified as Oops
import Control.Monad.Trans (lift)
import Data.Aeson qualified as Json
import Data.Map (Map)
import Data.Map.Lazy qualified as Map
import Data.Tagged (Tagged (unTagged))
import Data.Text qualified as Text
import Language.PureScript.CoreFn.FromJSON
  ( ModuleWithVersion
  , moduleWithoutVersion
  )
import Language.PureScript.CoreFn.Meta (Ann)
import Language.PureScript.CoreFn.Module (Module (moduleImports))
import Language.PureScript.CoreFn.ModuleName (ModuleName, moduleNameToText)
import Path
  ( Abs
  , Dir
  , File
  , Path
  , SomeBase (..)
  , mkRelFile
  , parseRelDir
  , toFilePath
  , (</>)
  )
import Path.IO (doesFileExist, makeAbsolute)

readModuleRecursively
  ∷ ∀ e
   . e `CouldBeAnyOf` '[ModuleNotFound, ModuleDecodingErr]
  ⇒ Tagged "output" (SomeBase Dir)
  → ModuleName
  → ExceptT (Oops.Variant e) IO (Map ModuleName (Module Ann))
readModuleRecursively output moduleName = recurse mempty [moduleName]
 where
  recurse
    ∷ Map ModuleName (Module Ann)
    → [ModuleName]
    → ExceptT (Oops.Variant e) IO (Map ModuleName (Module Ann))
  recurse loaded = \case
    [] → pure loaded
    modName : otherNames
      | "Prim" `Text.isPrefixOf` moduleNameToText modName →
          recurse loaded otherNames
    modName : otherNames
      | Map.member modName loaded →
          recurse loaded otherNames
    modName : otherNames →
      readModule output modName >>= \m →
        recurse
          (Map.insert modName m loaded)
          (otherNames <> (fmap snd . moduleImports) m)

readModule
  ∷ e `CouldBeAnyOf` '[ModuleNotFound, ModuleDecodingErr]
  ⇒ Tagged "output" (SomeBase Dir)
  → ModuleName
  → ExceptT (Variant e) IO (Module Ann)
readModule output modName = do
  path ← moduleAbsolutePath output modName
  lift (Json.eitherDecodeFileStrict @ModuleWithVersion (toFilePath path))
    >>= either (throw . ModuleDecodingErr path) (pure . moduleWithoutVersion)

moduleAbsolutePath
  ∷ e `CouldBe` ModuleNotFound
  ⇒ Tagged "output" (SomeBase Dir)
  → ModuleName
  → ExceptT (Variant e) IO (Path Abs File)
moduleAbsolutePath psOutPath modName = do
  psOutput ←
    case unTagged psOutPath of
      Abs a → pure a
      Rel r → makeAbsolute r
  prd ← parseRelDir (Text.unpack (moduleNameToText modName))
  let path = psOutput </> prd </> $(mkRelFile "corefn.json")
  exists ← doesFileExist path
  unless exists $ throw $ ModuleNotFound path
  pure path

--------------------------------------------------------------------------------
-- Errors ----------------------------------------------------------------------

newtype ModuleNotFound = ModuleNotFound (Path Abs File)
data ModuleDecodingErr = ModuleDecodingErr (Path Abs File) String
