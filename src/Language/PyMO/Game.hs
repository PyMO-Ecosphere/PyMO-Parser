{-# LANGUAGE OverloadedStrings #-}

module Language.PyMO.Game
  ( Game
    ( gameDir
    , gameConfig
    , gameScripts
    , gameCGAlbums
    , gameMusicList )
  , loadGame) where

import Language.PyMO.GameConfig
import Language.PyMO.Script
import Language.PyMO.MusicList
import Data.HashSet as HS
import Data.Maybe (mapMaybe)
import Data.Text (unpack)
import Data.List (nub)
import Language.PyMO.CGAlbum
import Control.Monad (forM)


data Game = Game
  { gameDir :: FilePath
  , gameConfig :: GameConfig
  , gameScripts :: [(ScriptName, Script)]
  , gameCGAlbums :: [CGAlbum]
  , gameMusicList :: Maybe MusicList }


instance Show Game where
  show g =
    "GameDir = " ++ gameDir g ++ "\n"
    ++ "= GameConfig =\n" ++ show (gameConfig g) ++ "\n"
    ++ "= Scripts (" ++ show (length $ gameScripts g) ++ ") =\n"
    ++ unlines (fst <$> gameScripts g) ++ "\n"
    ++ "= CG Albums (" ++ show (length $ gameCGAlbums g) ++ ") =\n"
    ++ unlines (show <$> gameCGAlbums g) ++ "\n"
    ++ "= Music List =\n" ++ show (gameMusicList g) ++ "\n"


loadScripts
  :: FilePath
  -> HashSet ScriptName
  -> [ScriptName]
  -> IO [(ScriptName, Script)]
loadScripts _ _ [] = pure []
loadScripts gameDir' scriptAlreadyLoaded (scriptName : nextScriptNames)
  | scriptName `HS.member` scriptAlreadyLoaded =
      loadScripts gameDir' scriptAlreadyLoaded nextScriptNames

  | otherwise = do
      script <- loadPyMOScript gameDir' scriptName
      let nextScriptNames2 = unpack <$> mapMaybe findNextScript script
          findNextScript (Stmt { stmtCommand = "change", stmtArgs = [s] }) =
            Just s
          findNextScript (Stmt { stmtCommand = "call", stmtArgs = [s] }) =
            Just s
          findNextScript _ = Nothing

      nextScripts <- loadScripts
        gameDir'
        (HS.insert scriptName scriptAlreadyLoaded)
        (nextScriptNames ++ nextScriptNames2)

      return $ (scriptName, script) : nextScripts


loadGame :: FilePath -> IO Game
loadGame gameDir' = do
  gc <- loadGameConfig $ gameDir' ++ "/gameconfig.txt"
  scripts <- loadScripts gameDir' empty [getStringValue "startscript" gc]
  let needToLoadMusicList = any $ any isMusicStmt
      isMusicStmt (Stmt { stmtCommand = "music", stmtArgs = [] })= True
      isMusicStmt _ = False
      albumLists = nub $ concatMap (mapMaybe findAlbumList . snd) scripts
      findAlbumList (Stmt { stmtCommand ="album", stmtArgs = []}) = Just Nothing
      findAlbumList (Stmt { stmtCommand = "album", stmtArgs = [a]}) = Just $ Just a
      findAlbumList _ = Nothing
      loadAlbum Nothing = loadDefaultCGAlbum gameDir'
      loadAlbum (Just x) = loadCGAlbum gameDir' $ unpack x

  albums <- forM albumLists loadAlbum

  musicList <-
    if needToLoadMusicList (snd <$> scripts)
      then Just <$> loadMusicList (gameDir' ++ "/script/music_list.txt")
      else pure Nothing

  return $ Game
    { gameDir = gameDir'
    , gameConfig = gc
    , gameScripts = scripts
    , gameCGAlbums = albums
    , gameMusicList = musicList }

