{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.CompanyFull.RecordLabel
  ( RecordLabel(..)
  , recordLabelSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype RecordLabel = RecordLabel Bool
  deriving (Show, Eq)

recordLabelSchema :: FC.Fleece t => FC.Schema t RecordLabel
recordLabelSchema =
  FC.coerceSchema FC.boolean