{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.PyMO.Utils
  ( loadText
  , commaCells
  , strip
  , lines
  , strippedLines
  , unCommaCells
  , loadStrippedLines
  , groupByKey
  , pattern IsInt
  , ToString (..)
  ) where

import Prelude hiding ( lines )
import Data.Text as T hiding (filter, lines, strip, partition)
import qualified Data.Text.IO.Utf8 as Utf8
import Text.Read (readMaybe)
import Data.List (partition)


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


strippedLines :: Text -> [Text]
strippedLines text = Prelude.filter (not . T.null) $ strip <$> lines text


loadStrippedLines :: FilePath -> IO [Text]
loadStrippedLines = fmap strippedLines . loadText


commaCells :: T.Text -> [T.Text]
commaCells x
  | T.null x = []
  | otherwise = T.splitOn "," x


unCommaCells :: [T.Text] -> T.Text
unCommaCells = T.intercalate ","


strip :: T.Text -> T.Text
strip = T.dropWhile isSpace . T.dropWhileEnd isSpace
  where isSpace ' ' = True
        isSpace '\t' = True
        isSpace '\r' = True
        isSpace _ = False


groupByKey :: Eq k => (a -> k) -> [a] -> [(k, [a])]
groupByKey _ [] = []
groupByKey f (x:xs) = (fx, x : sameKey) : groupByKey f differentKey
  where fx = f x
        (sameKey, differentKey) = partition ((== fx) . f) xs


class ToString a where
  toString :: a -> String


instance ToString Text where
  toString = unpack


instance ToString String where
  toString = id


pattern IsInt :: ToString a => Int -> a
pattern IsInt i <- (readMaybe . toString -> Just i)

