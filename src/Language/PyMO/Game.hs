module Language.PyMO.Game
  ( Game (..)
  ) where

import Language.PyMO.GameConfig (GameConfig, loadGameConfig)
import Language.PyMO.Script
import Language.PyMO.MusicList
import Data.HashSet (HashSet)

data Game = Game
  { gameDir :: FilePath
  , gameConfig :: GameConfig
  , gameScripts :: [(ScriptName, Script)]
  , gameMusicList :: Maybe MusicList }
  deriving ( Show )

loadGame :: FilePath -> IO Game
loadGame gameDir = do
  gameConfig <- loadGameConfig $ gameDir ++ "/gameconfig.txt"
  undefined