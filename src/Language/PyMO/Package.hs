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

import Data.Word (Word32, Word64)
import Data.ByteString.Lazy as B hiding (length, take, repeat)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import Data.Binary.Get ( getWord32le, runGet, getByteString, Get )
import Data.Text as T (Text, unpack, pack)
import Data.Text.Encoding ( decodeUtf8, encodeUtf8 )
import Control.Monad (replicateM, forM_, forM)
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import System.FilePath ( (</>), (<.>), takeBaseName )
import System.IO
import Data.ByteString.Builder
import Prelude hiding (replicate)


data FileEntry = FileEntry
  { fileName :: Text
  , offsetInPackage :: Word32
  , fileLength :: Word32 }
  deriving (Show)


data PackageReader = PackageReader
  { files :: [FileEntry]
  , handle :: Handle
  , baseOffset :: Word64 }


instance Show PackageReader where
  show (PackageReader files' _ _) = show files'


openPackage :: FilePath -> ResourceT IO PackageReader
openPackage filePath = do
  h <- liftIO $ openBinaryFile filePath ReadMode
  _ <- register $ hClose h
  liftIO $ hSeek h AbsoluteSeek 0
  fileCountBS <- liftIO $ BS.hGet h 4
  let fileCount = runGet getWord32le (B.fromStrict fileCountBS)
  let entrySize = fromIntegral fileCount * (32 + 4 + 4)
  entriesBS <- liftIO $ BS.hGet h entrySize
  let getFileEntries :: Get [FileEntry]
      getFileEntries = replicateM (fromIntegral fileCount) getFileEntry
      getFileEntry :: Get FileEntry
      getFileEntry = do
        fileNameBS <- getByteString 32
        offset <- getWord32le
        length' <- getWord32le
        return $ FileEntry
          { fileName = decodeUtf8 $ BS.takeWhile (/= 0) fileNameBS
          , offsetInPackage = offset
          , fileLength = length' }
  let files' = runGet getFileEntries (B.fromStrict entriesBS)
  return $ PackageReader
    { files = files'
    , handle = h
    , baseOffset = 0 }


getFile :: PackageReader -> FileEntry -> IO LazyByteString
getFile packageReader fileEntry = do
  let h = handle packageReader
      base = baseOffset packageReader
      offset = fromIntegral (offsetInPackage fileEntry) + base
      len = fromIntegral (fileLength fileEntry)
  hSeek h AbsoluteSeek (fromIntegral offset)
  BS.hGet h len >>= return . B.fromStrict


extractFile :: PackageReader -> FileEntry -> FilePath -> IO ()
extractFile packageReader fileEntry outputPath = do
  bytes <- getFile packageReader fileEntry
  B.writeFile outputPath bytes


extractAllFiles :: PackageReader -> FilePath -> String -> IO ()
extractAllFiles packageReader outDir fileExt =
  forM_ (files packageReader) $ \fileEntry ->
    extractFile packageReader fileEntry $
      outDir </> T.unpack (fileName fileEntry) <.> fileExt


extractPackage :: FilePath -> FilePath -> String -> IO ()
extractPackage packagePath outDir fileExt = runResourceT $ do
  pkg <- openPackage packagePath
  liftIO $ extractAllFiles pkg outDir fileExt


packByteStrings :: [(String, LazyByteString)] -> LazyByteString
packByteStrings filesToPack = toLazyByteString $ mconcat
  [ word32LE $ fromIntegral $ length filesToPack
  , fileHeader baseOffset' filesToPack
  , mconcat $ fmap (lazyByteString . snd) filesToPack ]
  where baseOffset' = fromIntegral $ 4 + (32 + 4 + 4) * length filesToPack
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

