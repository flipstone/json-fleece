{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.LiteratureBase.Report
  ( Report(..)
  , reportSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Report = Report Bool
  deriving (Show, Eq)

reportSchema :: FC.Fleece schema => schema Report
reportSchema =
  FC.coerceSchema FC.boolean