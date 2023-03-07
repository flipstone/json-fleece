{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TradingCardSetFull.ReleaseDay
  ( ReleaseDay(..)
  , releaseDaySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype ReleaseDay = ReleaseDay Integer
  deriving (Show, Eq)

releaseDaySchema :: FC.Fleece schema => schema ReleaseDay
releaseDaySchema =
  FC.coerceSchema FC.integer