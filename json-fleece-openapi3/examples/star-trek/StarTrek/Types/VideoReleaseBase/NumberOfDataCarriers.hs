{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.VideoReleaseBase.NumberOfDataCarriers
  ( NumberOfDataCarriers(..)
  , numberOfDataCarriersSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype NumberOfDataCarriers = NumberOfDataCarriers Integer
  deriving (Show, Eq)

numberOfDataCarriersSchema :: FC.Fleece schema => schema NumberOfDataCarriers
numberOfDataCarriersSchema =
  FC.coerceSchema FC.integer