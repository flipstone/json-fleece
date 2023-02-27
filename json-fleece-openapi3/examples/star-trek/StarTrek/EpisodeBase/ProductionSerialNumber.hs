{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.EpisodeBase.ProductionSerialNumber
  ( ProductionSerialNumber(..)
  , productionSerialNumberSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype ProductionSerialNumber = ProductionSerialNumber Text
  deriving (Show, Eq)

productionSerialNumberSchema :: FC.Fleece schema => schema ProductionSerialNumber
productionSerialNumberSchema =
  FC.coerceSchema FC.text