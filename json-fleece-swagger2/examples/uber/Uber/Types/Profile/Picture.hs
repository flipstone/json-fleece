{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Types.Profile.Picture
  ( Picture(..)
  , pictureSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Picture = Picture T.Text
  deriving (Show, Eq)

pictureSchema :: FC.Fleece schema => schema Picture
pictureSchema =
  FC.coerceSchema FC.text