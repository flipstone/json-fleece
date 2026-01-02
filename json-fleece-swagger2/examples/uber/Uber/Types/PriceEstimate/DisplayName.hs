{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Types.PriceEstimate.DisplayName
  ( DisplayName(..)
  , displayNameSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype DisplayName = DisplayName T.Text
  deriving (Show, Eq)

displayNameSchema :: FC.Fleece t => FC.Schema t DisplayName
displayNameSchema =
  FC.coerceSchema FC.text