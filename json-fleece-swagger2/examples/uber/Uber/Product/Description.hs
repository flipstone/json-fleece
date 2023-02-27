{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Product.Description
  ( Description(..)
  , descriptionSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Description = Description Text
  deriving (Show, Eq)

descriptionSchema :: FC.Fleece schema => schema Description
descriptionSchema =
  FC.coerceSchema FC.text