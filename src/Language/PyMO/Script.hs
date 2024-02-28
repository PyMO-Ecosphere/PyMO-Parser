{-# LANGUAGE OverloadedStrings #-}

module Language.PyMO.Script
  ( Stmt (..)
  , Script
  , loadPyMOScript
  ) where

import Data.Text as T
import Language.PyMO.Utils as U
import Data.Maybe (catMaybes)

data Stmt = Stmt
  { stmtCommand :: Text
  , stmtArgs :: [Text]
  , stmtNextLines :: [Text]
  , stmtScriptName :: String
  , stmtLineNumber :: Int
  } deriving ( Show )

type Script = [Stmt]

parseStmt :: String -> Int -> Text -> [Text] -> Maybe Stmt
parseStmt scriptName lineNumber line nextLines
  | not $ "#" `T.isPrefixOf` line = Nothing
  | otherwise =
    let (cmd, arg) = T.breakOn " " $ T.drop 1 line in
    Just $ Stmt cmd (commaCells $ U.strip arg) nextLines scriptName lineNumber

removeComment :: Text -> Text
removeComment = T.dropWhileEnd (== ';')

mapWithRemain :: (a -> [a] -> b) -> [a] -> [b]
mapWithRemain _ [] = []
mapWithRemain f (x:xs) = f x xs : mapWithRemain f xs

mapMaybeWithRemain :: (a -> [a] -> Maybe b) -> [a] -> [b]
mapMaybeWithRemain f xs = catMaybes $ mapWithRemain f xs

parsePyMOScript :: String -> Text -> Script
parsePyMOScript scriptName content =
  flip mapMaybeWithRemain (Prelude.zip [1..] $ U.lines content) $
    \(lineNumber, line) xs ->
      parseStmt scriptName lineNumber (U.strip $ removeComment $ U.strip line) $
        fmap snd xs

loadPyMOScript :: String -> String -> IO Script
loadPyMOScript gameDir scriptName = do
  content <- loadText $ gameDir ++ "/script/" ++ scriptName ++ ".txt"
  return $ parsePyMOScript scriptName content
