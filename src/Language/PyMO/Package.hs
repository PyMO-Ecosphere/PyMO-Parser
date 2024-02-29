module Language.PyMO.Package
  ( FileEntry
    ( fileName
    , fileLength )
  , PackageReader ( files )
  , openPackage
  , getFile
  , extractFile
  , extractAllFiles
  , extractPackage
  ) where

import Data.Word (Word32)
import Data.ByteString.Lazy as B
import Data.ByteString as BS ( takeWhile )
import Data.Binary.Get ( getWord32le, runGet, getByteString, Get )
import Data.Text as T (Text, unpack)
import Data.Text.Encoding ( decodeUtf8 )
import Control.Monad (replicateM, forM_)


data FileEntry = FileEntry
  { fileName :: Text
  , offsetInPackage :: Word32
  , fileLength :: Word32 }
  deriving (Show)


data PackageReader = PackageReader
  { files :: [FileEntry]
  , bytes :: LazyByteString }


instance Show PackageReader where
  show (PackageReader files' _) = show files'


openPackage :: FilePath -> IO PackageReader
openPackage filePath = do
  bytes' <- B.readFile filePath
  let getFileEntries :: Get [FileEntry]
      getFileEntries = do
        fileCount <- fromIntegral <$> getWord32le
        replicateM fileCount getFileEntry
      getFileEntry :: Get FileEntry
      getFileEntry = do
        fileNameBS <- getByteString 32
        offset <- getWord32le
        length' <- getWord32le
        return $ FileEntry
          { fileName = decodeUtf8 $ BS.takeWhile (/= 0) fileNameBS
          , offsetInPackage = offset
          , fileLength = length' }

  return $ PackageReader
    { files = runGet getFileEntries bytes'
    , bytes = bytes' }


getFile :: PackageReader -> FileEntry -> LazyByteString
getFile packageReader fileEntry =
  B.take (fromIntegral $ fileLength fileEntry) $
    B.drop (fromIntegral $ offsetInPackage fileEntry) $
      bytes packageReader


extractFile :: PackageReader -> FileEntry -> FilePath -> IO ()
extractFile packageReader fileEntry outputPath = do
  B.writeFile outputPath $ getFile packageReader fileEntry


extractAllFiles :: PackageReader -> FilePath -> String -> IO ()
extractAllFiles packageReader outDir fileExt =
  forM_ (files packageReader) $ \fileEntry ->
    extractFile packageReader fileEntry $
      outDir ++ "/" ++ T.unpack (fileName fileEntry) ++ fileExt


extractPackage :: FilePath -> FilePath -> String -> IO ()
extractPackage packagePath outDir fileExt = do
  pkg <- openPackage packagePath
  extractAllFiles pkg outDir fileExt

