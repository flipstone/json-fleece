{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Profile.Picture
  ( Picture(..)
  , pictureSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Picture = Picture Text
  deriving (Show, Eq)

pictureSchema :: FC.Fleece schema => schema Picture
pictureSchema =
  FC.coerceSchema FC.text