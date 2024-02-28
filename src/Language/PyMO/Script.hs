{-# LANGUAGE OverloadedStrings, PatternSynonyms #-}

module Language.PyMO.Script
  ( Stmt
    ( stmtCommand
    , stmtArgs
    , stmtNextLines
    , stmtScriptName
    , stmtLineNumber )
  , pattern Stmt
  , ScriptName
  , Script
  , loadPyMOScript
  ) where

import Data.Text as T
import Language.PyMO.Utils as U
import Data.Maybe (catMaybes)


type ScriptName = String


data Stmt = Stmt
  { stmtCommand :: Text
  , stmtArgs :: [Text]
  , stmtNextLines :: [Text]
  , stmtScriptName :: ScriptName
  , stmtLineNumber :: Int
  }


instance Show Stmt where
  show stmt =
    "#" ++ T.unpack (stmtCommand stmt) ++ " "
    ++ T.unpack (T.intercalate "," $ stmtArgs stmt)


type Script = [Stmt]


parseStmt :: ScriptName -> Int -> Text -> [Text] -> Maybe Stmt
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


parsePyMOScript :: ScriptName -> Text -> Script
parsePyMOScript scriptName content =
  flip mapMaybeWithRemain (Prelude.zip [1..] $ U.lines content) $
    \(lineNumber, line) xs ->
      parseStmt scriptName lineNumber (U.strip $ removeComment $ U.strip line) $
        fmap snd xs


loadPyMOScript :: FilePath -> ScriptName -> IO Script
loadPyMOScript gameDir scriptName = do
  content <- loadText $ gameDir ++ "/script/" ++ scriptName ++ ".txt"
  return $ parsePyMOScript scriptName content

