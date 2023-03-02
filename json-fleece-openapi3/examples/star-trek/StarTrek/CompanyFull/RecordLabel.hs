{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CompanyFull.RecordLabel
  ( RecordLabel(..)
  , recordLabelSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype RecordLabel = RecordLabel Bool
  deriving (Show, Eq)

recordLabelSchema :: FC.Fleece schema => schema RecordLabel
recordLabelSchema =
  FC.coerceSchema FC.boolean