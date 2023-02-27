{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Product.Image
  ( Image(..)
  , imageSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Image = Image Text
  deriving (Show, Eq)

imageSchema :: FC.Fleece schema => schema Image
imageSchema =
  FC.coerceSchema FC.text