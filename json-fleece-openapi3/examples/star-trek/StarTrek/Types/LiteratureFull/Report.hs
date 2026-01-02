{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.LiteratureFull.Report
  ( Report(..)
  , reportSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Report = Report Bool
  deriving (Show, Eq)

reportSchema :: FC.Fleece t => FC.Schema t Report
reportSchema =
  FC.coerceSchema FC.boolean