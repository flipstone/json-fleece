{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.EpisodeBase.ProductionSerialNumber
  ( ProductionSerialNumber(..)
  , productionSerialNumberSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype ProductionSerialNumber = ProductionSerialNumber T.Text
  deriving (Show, Eq)

productionSerialNumberSchema :: FC.Fleece schema => schema ProductionSerialNumber
productionSerialNumberSchema =
  FC.coerceSchema FC.text