{-# LANGUAGE NoImplicitPrelude #-}

module Uber.PriceEstimate.DisplayName
  ( DisplayName(..)
  , displayNameSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype DisplayName = DisplayName Text
  deriving (Show, Eq)

displayNameSchema :: FC.Fleece schema => schema DisplayName
displayNameSchema =
  FC.coerceSchema FC.text