{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Types.Product.DisplayName
  ( DisplayName(..)
  , displayNameSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype DisplayName = DisplayName T.Text
  deriving (Show, Eq)

displayNameSchema :: FC.Fleece schema => schema DisplayName
displayNameSchema =
  FC.coerceSchema FC.text