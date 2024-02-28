{-# LANGUAGE OverloadedStrings #-}

module Language.PyMO.Utils
  ( loadText
  , loadTextLines
  , commaCells
  , strip
  , lines
  , unCommaCells ) where

import Prelude hiding ( lines )
import Data.Text as T hiding ( lines, strip )
import qualified Data.Text.IO.Utf8 as Utf8

loadText :: FilePath -> IO Text
loadText filePath = do
  content <- Utf8.readFile filePath
  if "\65279" `T.isPrefixOf` content
    then return $ T.tail content
    else return content

lines :: Text -> [Text]
lines text =
  T.splitOn "\r\n" text
  >>= T.splitOn "\r"
  >>= T.splitOn "\n"

loadTextLines :: FilePath -> IO [Text]
loadTextLines = fmap lines . loadText

commaCells :: T.Text -> [T.Text]
commaCells x
  | T.null x = []
  | otherwise = T.splitOn "," x

unCommaCells :: [T.Text] -> T.Text
unCommaCells = T.intercalate ","

strip :: T.Text -> T.Text
strip = T.dropWhile isSpace . T.dropWhileEnd isSpace
  where
    isSpace ' ' = True
    isSpace '\t' = True
    isSpace '\r' = True
    isSpace _ = False

