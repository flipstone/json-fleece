{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TradingCardSetBase.ReleaseDay
  ( ReleaseDay(..)
  , releaseDaySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype ReleaseDay = ReleaseDay Integer
  deriving (Show, Eq)

releaseDaySchema :: FC.Fleece t => FC.Schema t ReleaseDay
releaseDaySchema =
  FC.coerceSchema FC.integer