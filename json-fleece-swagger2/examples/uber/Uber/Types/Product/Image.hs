{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Types.Product.Image
  ( Image(..)
  , imageSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Image = Image T.Text
  deriving (Show, Eq)

imageSchema :: FC.Fleece t => FC.Schema t Image
imageSchema =
  FC.coerceSchema FC.text