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
  , packFiles
  ) where

import Data.Word (Word32)
import Data.ByteString.Lazy as B hiding (length, take, repeat)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import Data.Binary.Get ( getWord32le, runGet, getByteString, Get )
import Data.Text as T (Text, unpack, pack)
import Data.Text.Encoding ( decodeUtf8, encodeUtf8 )
import Control.Monad (replicateM, forM_, forM)
import System.FilePath ( (</>), (<.>), takeBaseName )
import Data.ByteString.Builder
import Prelude hiding (replicate)


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
      outDir </> T.unpack (fileName fileEntry) <.> fileExt


extractPackage :: FilePath -> FilePath -> String -> IO ()
extractPackage packagePath outDir fileExt = do
  pkg <- openPackage packagePath
  extractAllFiles pkg outDir fileExt


packByteStrings :: [(String, LazyByteString)] -> LazyByteString
packByteStrings filesToPack = toLazyByteString $ mconcat
  [ word32LE $ fromIntegral $ length filesToPack
  , fileHeader baseOffset filesToPack
  , mconcat $ fmap (lazyByteString . snd) filesToPack ]
  where baseOffset = fromIntegral $ 4 + (32 + 4 + 4) * length filesToPack
        fileHeader _ [] = mempty
        fileHeader offset (x : xs) =
          mappend (fileEntry (fst x) offset $ fromIntegral $ B.length $ snd x) $
            fileHeader (offset + fromIntegral (B.length $ snd x)) xs
        fileEntry name offset len =
          mconcat [ convertStr name, word32LE offset, word32LE len ]
        convertStr text =
          padStr $ BS.take 32 $ encodeUtf8 $ T.pack text
        padStr str =
          mappend (byteString str) $ byteString $
            BS.replicate (32 - BS.length str) 0


packFiles :: [FilePath] -> FilePath -> IO ()
packFiles filesToPack outPath = do
  files' <- forM filesToPack $ \filePath -> do
    binary <- B.readFile filePath
    return (takeBaseName filePath, binary)
  B.writeFile outPath $ packByteStrings files'

