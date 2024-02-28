{-# LANGUAGE OverloadedStrings #-}

module Language.PyMO.MusicList
  ( MusicFileName
  , MusicTitle
  , MusicList
  , loadMusicList
  ) where

import Data.Text as T
import Language.PyMO.Utils as U
import Data.Maybe (catMaybes)


type MusicFileName = Text
type MusicTitle = Text
type MusicList = [(MusicFileName, MusicTitle)]


parseSingleMusic :: Text -> IO (Maybe (MusicFileName, MusicTitle))
parseSingleMusic line = do
  let (filename, title) = breakOn "," line
      filename' = U.strip filename
      title' = U.strip title

  if T.null filename'
    then do
      putStrLn "[Warning] Can not find music filename."
      return Nothing
    else
      return $ Just (filename', title')


parseMusicList :: Text -> IO MusicList
parseMusicList musicList =
  catMaybes <$> mapM parseSingleMusic (T.lines musicList)


loadMusicList :: FilePath -> IO MusicList
loadMusicList x = U.loadText x >>= parseMusicList

