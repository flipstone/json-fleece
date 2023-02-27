{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardSetBase.ReleaseDay
  ( ReleaseDay(..)
  , releaseDaySchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype ReleaseDay = ReleaseDay Integer
  deriving (Show, Eq)

releaseDaySchema :: FC.Fleece schema => schema ReleaseDay
releaseDaySchema =
  FC.coerceSchema FC.integer