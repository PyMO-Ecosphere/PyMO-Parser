{-# LANGUAGE DeriveGeneric #-}

module Language.PyMO.AssetDatabase
  ( AssetKind(..)
  , AssetName
  , AssetNameLowered
  , AssetDatabase
  , openAssetDatabase
  , AssetReference
    ( arName
    , arNameLowered
    , arKind )
  , getAssetRef
  , getAssetBytes
  ) where

import Control.Monad.Trans.Resource (ResourceT)
import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower, toUpper)
import Data.Hashable (Hashable)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import qualified Data.List as List
import System.FilePath ((</>))
import System.Directory (doesFileExist)

import Language.PyMO.GameConfig (GameConfig, getStringValue)
import Language.PyMO.Package
  ( FileEntry(fileName)
  , PackageReader(files)
  , openPackage
  , getFile )
import GHC.Generics (Generic)


-- | Types of assets in PyMO games
data AssetKind
  = Bg
  | Bgm
  | Chara
  | Se
  | System
  | Video
  | Voice
  deriving (Show, Eq, Generic)

instance Hashable AssetKind

type AssetName = String
type AssetNameLowered = String

-- | Internal: Asset source location (not exported)
data AssetSource
  = Unpacked FilePath                 -- ^ Individual file on disk
  | Packed (PackageReader, FileEntry) -- ^ File inside a package

-- | Database of game assets with optional package readers for packed assets
data AssetDatabase = AssetDatabase
  { -- Internal: Package readers for packed assets (not exported)
    _adBg :: Maybe PackageReader
  , _adChara :: Maybe PackageReader
  , _adSe :: Maybe PackageReader
  , _adVoice :: Maybe PackageReader
  , _adGameConfig :: GameConfig
  , _adGameDir :: FilePath
  }

-- | Reference to a specific asset
data AssetReference = AssetReference
  { arName :: AssetName           -- ^ Original asset name
  , arNameLowered :: AssetNameLowered  -- ^ Lowercase asset name for case-insensitive lookup
  , arKind :: AssetKind           -- ^ Type of asset
    -- Internal: Source location (not exported)
  , _arSource :: AssetSource
  }

-- | Open an asset database for the given game directory and configuration
openAssetDatabase :: FilePath -> GameConfig -> ResourceT IO AssetDatabase
openAssetDatabase gameDir gameConfig = do
  -- Try to open package files for packable asset types
  bgReader <- openPackageIfExists (gameDir </> "bg" </> "bg.pak")
  charaReader <- openPackageIfExists (gameDir </> "chara" </> "chara.pak")
  seReader <- openPackageIfExists (gameDir </> "se" </> "se.pak")
  voiceReader <- openPackageIfExists (gameDir </> "voice" </> "voice.pak")

  return $ AssetDatabase
    { _adBg = bgReader
    , _adChara = charaReader
    , _adSe = seReader
    , _adVoice = voiceReader
    , _adGameConfig = gameConfig
    , _adGameDir = gameDir
    }
  where
    openPackageIfExists :: FilePath -> ResourceT IO (Maybe PackageReader)
    openPackageIfExists path = do
      exists <- liftIO $ doesFileExist path
      if exists
        then Just <$> openPackage path
        else return Nothing

-- | Get a reference to an asset if it exists (requires IO to check filesystem)
getAssetRef :: AssetDatabase -> AssetKind -> AssetName -> IO (Maybe AssetReference)
getAssetRef db kind name =
  let nameLowered = map toLower name
  in case kind of
    -- Packable assets: check package first, then individual files
    Bg -> getAssetRef' db name nameLowered kind _adBg "bg" "bgformat"
    Chara -> getAssetRef' db name nameLowered kind _adChara "chara" "charaformat"
    Se -> getAssetRef' db name nameLowered kind _adSe "se" "seformat"
    Voice -> getAssetRef' db name nameLowered kind _adVoice "voice" "voiceformat"

    -- Non-packable assets: always individual files
    Bgm -> getUnpackedAssetRef db name nameLowered kind "bgm" "bgmformat"
    System -> getUnpackedAssetRef db name nameLowered kind "system" "systemformat"
    Video -> getUnpackedAssetRef db name nameLowered kind "video" "videoformat"

-- | Internal helper for packable assets
getAssetRef' :: AssetDatabase -> AssetName -> AssetNameLowered -> AssetKind
             -> (AssetDatabase -> Maybe PackageReader) -> String -> String -> IO (Maybe AssetReference)
getAssetRef' db name nameLowered kind getReader subDir formatKey =
  case getReader db of
    -- Check package first
    Just pkg -> case findInPackage pkg name of
      Just entry -> return $ Just $ AssetReference
        { arName = name
        , arNameLowered = nameLowered
        , arKind = kind
        , _arSource = Packed (pkg, entry)
        }
      Nothing -> return Nothing

    -- Fall back to individual file
    Nothing -> getUnpackedAssetRef db name nameLowered kind subDir formatKey

-- | Internal helper for unpacked assets
getUnpackedAssetRef :: AssetDatabase -> AssetName -> AssetNameLowered -> AssetKind
                    -> String -> String -> IO (Maybe AssetReference)
getUnpackedAssetRef db name nameLowered kind subDir formatKey = do
  let format = getStringValue (T.pack formatKey) (_adGameConfig db)
      ext = case format of
             "" -> ""
             ('.':_) -> format
             _ -> "." ++ format
      filePath = _adGameDir db </> subDir </> name ++ ext
  exists <- doesFileExist filePath
  if exists
    then return $ Just $ AssetReference
      { arName = name
      , arNameLowered = nameLowered
      , arKind = kind
      , _arSource = Unpacked filePath
      }
    else return Nothing

-- | Find a file entry in a package (case-insensitive, pure function)
findInPackage :: PackageReader -> AssetName -> Maybe FileEntry
findInPackage pkg name =
  let targetUpper = map toUpper name
  in List.find (\entry -> map toUpper (T.unpack (fileName entry)) == targetUpper) (files pkg)

-- | Read the bytes of an asset
getAssetBytes :: AssetReference -> IO B.ByteString
getAssetBytes ref = case _arSource ref of
  Unpacked path -> B.readFile path
  Packed (pkg, entry) -> getFile pkg entry
