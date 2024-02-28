module Language.PyMO.CGAlbum
  ( CG (..)
  ) where

import Data.Text


data CG = CG
  { cgAlbumName' :: String
  , cgLineNumber :: Int
  , cgPage :: Int
  , cgTitle :: Text
  , cgFiles :: [Text]
  , cgForceUnlocked :: Bool
  } deriving (Show)
