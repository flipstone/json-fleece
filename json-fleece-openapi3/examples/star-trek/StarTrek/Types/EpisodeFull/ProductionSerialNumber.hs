{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.EpisodeFull.ProductionSerialNumber
  ( ProductionSerialNumber(..)
  , productionSerialNumberSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype ProductionSerialNumber = ProductionSerialNumber T.Text
  deriving (Show, Eq)

productionSerialNumberSchema :: FC.Fleece t => FC.Schema t ProductionSerialNumber
productionSerialNumberSchema =
  FC.coerceSchema FC.text