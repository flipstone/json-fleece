{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Country.Iso31661Alpha2Code
  ( Iso31661Alpha2Code(..)
  , iso31661Alpha2CodeSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Iso31661Alpha2Code = Iso31661Alpha2Code Text
  deriving (Show, Eq)

iso31661Alpha2CodeSchema :: FC.Fleece schema => schema Iso31661Alpha2Code
iso31661Alpha2CodeSchema =
  FC.coerceSchema FC.text