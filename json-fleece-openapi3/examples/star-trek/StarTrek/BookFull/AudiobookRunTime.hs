{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookFull.AudiobookRunTime
  ( AudiobookRunTime(..)
  , audiobookRunTimeSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype AudiobookRunTime = AudiobookRunTime Integer
  deriving (Show, Eq)

audiobookRunTimeSchema :: FC.Fleece schema => schema AudiobookRunTime
audiobookRunTimeSchema =
  FC.coerceSchema FC.integer