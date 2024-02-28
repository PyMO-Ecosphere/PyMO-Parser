{-# LANGUAGE TemplateHaskell #-}

module Language.PyMO.GameConfig
  ( GameConfig
  , loadGameConfig
  , writeGameConfig
  , getTextValue
  , getStringValue
  , getIntValue
  , getInt2Value ) where

import Data.FileEmbed (embedStringFile)
import Data.HashMap.Strict as HM hiding (mapMaybe)
import Data.Maybe (mapMaybe)
import Data.Text as T
import Language.PyMO.Utils as U
import qualified Data.Text.IO.Utf8


type GameConfigKey = Text


newtype GameConfig = GameConfig (HashMap GameConfigKey [Text])


instance Show GameConfig where
  show = unpack . generateGameConfigText


generateGameConfigText :: GameConfig -> Text
generateGameConfigText (GameConfig gc) =
  T.unlines (convert <$> toList gc)
  where convert (k, v) = unCommaCells (k : v)


writeGameConfig :: FilePath -> GameConfig -> IO ()
writeGameConfig filePath gc =
  Data.Text.IO.Utf8.writeFile filePath $ generateGameConfigText gc


parseGameConfig :: Text -> GameConfig
parseGameConfig =
  GameConfig . fromList . parseLines . fmap U.strip . U.lines
  where parseLines = mapMaybe parseLine
        parseLine = t . commaCells . U.strip
        t [] = Nothing
        t (x : xs) = Just (x, xs)


loadGameConfig :: FilePath -> IO GameConfig
loadGameConfig = fmap parseGameConfig . loadText


defaultGameConfig :: GameConfig
defaultGameConfig = parseGameConfig
  $(embedStringFile "./src/Language/PyMO/default-gameconfig.txt")


getValues :: GameConfigKey -> GameConfig -> [Text]
getValues key (GameConfig gc) =
  case HM.lookup key gc of
    Just x -> x
    Nothing ->
      let (GameConfig dgc) = defaultGameConfig in
      case HM.lookup key dgc of
        Just x -> x
        Nothing -> error $ "Failed to get game config value: " ++ unpack key


getTextValue :: GameConfigKey -> GameConfig -> Text
getTextValue key gc =
  case getValues key gc of
    [x] -> x
    _ -> error $ "Invalid game config value: " ++ unpack key


getStringValue :: GameConfigKey -> GameConfig -> String
getStringValue gc = unpack . getTextValue gc


getIntValue :: GameConfigKey -> GameConfig -> Int
getIntValue gc = read . getStringValue gc


getInt2Value :: GameConfigKey -> GameConfig -> (Int, Int)
getInt2Value key gc =
  case getValues key gc of
    [x, y] -> (read (unpack x), read (unpack y))
    _ -> error $ "Invalid game config value: " ++ unpack key

