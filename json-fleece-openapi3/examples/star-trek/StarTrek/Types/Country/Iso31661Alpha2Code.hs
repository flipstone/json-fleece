{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.Country.Iso31661Alpha2Code
  ( Iso31661Alpha2Code(..)
  , iso31661Alpha2CodeSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Iso31661Alpha2Code = Iso31661Alpha2Code T.Text
  deriving (Show, Eq)

iso31661Alpha2CodeSchema :: FC.Fleece t => FC.Schema t Iso31661Alpha2Code
iso31661Alpha2CodeSchema =
  FC.coerceSchema FC.text