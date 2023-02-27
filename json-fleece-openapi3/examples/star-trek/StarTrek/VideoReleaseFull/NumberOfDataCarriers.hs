{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoReleaseFull.NumberOfDataCarriers
  ( NumberOfDataCarriers(..)
  , numberOfDataCarriersSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype NumberOfDataCarriers = NumberOfDataCarriers Integer
  deriving (Show, Eq)

numberOfDataCarriersSchema :: FC.Fleece schema => schema NumberOfDataCarriers
numberOfDataCarriersSchema =
  FC.coerceSchema FC.integer