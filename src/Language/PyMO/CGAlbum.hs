{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Language.PyMO.CGAlbum
  ( CG
    ( cgPage
    , cgTitle
    , cgFiles
    , cgForcedUnlocked )
  , CGAlbum
    ( cgaListFileName
    , cgaAlbumBgPrefix )
  , loadDefaultCGAlbum
  , loadCGAlbum
  , maxPageId
  , getPage) where

import Data.Text (Text, unpack)
import Language.PyMO.Utils
import Language.PyMO.Script (ScriptName)
import Control.Monad (forM)
import Data.Maybe (catMaybes)


data CG = CG
  { cgPage :: Int
  , cgTitle :: Text
  , cgFiles :: [Text]
  , cgForcedUnlocked :: Bool
  } deriving (Show)


loadCG :: Text -> Either String CG
loadCG line = parse $ commaCells line
  where parse (IsInt page : IsInt count : title : next) = Right $ CG
          { cgPage = page
          , cgTitle = title
          , cgFiles = take count next
          , cgForcedUnlocked = drop count next == ["1"]  }
        parse x = Left $ "Invalid CG line: " ++ unpack (unCommaCells x)


data CGAlbum = CGAlbum
  { cgaCGs :: [(Int, [CG])]
  , cgaListFileName :: ScriptName
  , cgaAlbumBgPrefix :: String }
  deriving (Show)


maxPageId :: CGAlbum -> Int
maxPageId cgAlbum
  | null $ cgaCGs cgAlbum = 0
  | otherwise = maximum (fst <$> cgaCGs cgAlbum)


getPage :: Int -> CGAlbum -> Maybe [CG]
getPage pageId cgAlbum = lookup pageId $ cgaCGs cgAlbum


loadCGAlbumInternal :: FilePath -> ScriptName -> String -> IO CGAlbum
loadCGAlbumInternal gameDir listFileName bgPrefix = do
  file <- loadStrippedLines $ gameDir ++ "/script/" ++ listFileName ++ ".txt"
  cgs' <- fmap catMaybes $ forM (fmap loadCG file) $ \case
    Left err -> putStrLn err >> return Nothing
    Right x -> return $ Just x
  return $ CGAlbum
    { cgaCGs = groupByKey cgPage cgs'
    , cgaListFileName = listFileName
    , cgaAlbumBgPrefix = bgPrefix }


loadDefaultCGAlbum :: FilePath -> IO CGAlbum
loadDefaultCGAlbum gameDir = loadCGAlbumInternal gameDir "album_list" "albumbg"


loadCGAlbum :: FilePath -> ScriptName -> IO CGAlbum
loadCGAlbum gameDir listFileName =
  loadCGAlbumInternal gameDir listFileName listFileName

