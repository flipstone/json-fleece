{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Product.Image
  ( Image(..)
  , imageSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Image = Image T.Text
  deriving (Show, Eq)

imageSchema :: FC.Fleece schema => schema Image
imageSchema =
  FC.coerceSchema FC.text