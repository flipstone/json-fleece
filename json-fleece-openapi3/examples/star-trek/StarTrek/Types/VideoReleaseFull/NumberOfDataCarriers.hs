{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.VideoReleaseFull.NumberOfDataCarriers
  ( NumberOfDataCarriers(..)
  , numberOfDataCarriersSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype NumberOfDataCarriers = NumberOfDataCarriers Integer
  deriving (Show, Eq)

numberOfDataCarriersSchema :: FC.Fleece t => FC.Schema t NumberOfDataCarriers
numberOfDataCarriersSchema =
  FC.coerceSchema FC.integer