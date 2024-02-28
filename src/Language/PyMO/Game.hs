{-# LANGUAGE OverloadedStrings #-}

module Language.PyMO.Game
  ( Game (..)
  , loadGame) where

import Language.PyMO.GameConfig
import Language.PyMO.Script
import Language.PyMO.MusicList
import Data.HashSet as HS
import Data.Maybe (mapMaybe)
import Data.Text (unpack)


data Game = Game
  { gameDir :: FilePath
  , gameConfig :: GameConfig
  , gameScripts :: [(ScriptName, Script)]
  , gameMusicList :: Maybe MusicList }


instance Show Game where
  show g =
    "GameDir = " ++ gameDir g ++ "\n"
    ++ "= GameConfig =\n" ++ show (gameConfig g) ++ "\n"
    ++ "= Scripts (" ++ show (length $ gameScripts g) ++ ") =\n"
    ++ unlines (fst <$> gameScripts g) ++ "\n"
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
          findNextScript (Stmt "change" [s] _ _ _) = Just s
          findNextScript (Stmt "call" [s] _ _ _) = Just s
          findNextScript _ = Nothing
          allNextScripts = nextScriptNames ++ nextScriptNames2

      nextScripts <- loadScripts
        gameDir'
        (HS.insert scriptName scriptAlreadyLoaded)
        allNextScripts

      return $ (scriptName, script) : nextScripts


loadGame :: FilePath -> IO Game
loadGame gameDir' = do
  gc <- loadGameConfig $ gameDir' ++ "/gameconfig.txt"
  scripts <- loadScripts gameDir' empty [getStringValue "startscript" gc]
  let needToLoadMusicList = any $ any isMusicStmt
      isMusicStmt (Stmt { stmtCommand = "music" })= True
      isMusicStmt _ = False

  musicList <-
    if needToLoadMusicList (snd <$> scripts)
      then Just <$> loadMusicList (gameDir' ++ "/script/music_list.txt")
      else pure Nothing

  return $ Game
    { gameDir = gameDir'
    , gameConfig = gc
    , gameScripts = scripts
    , gameMusicList = musicList }

