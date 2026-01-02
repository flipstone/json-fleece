{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.BookFull.AudiobookRunTime
  ( AudiobookRunTime(..)
  , audiobookRunTimeSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype AudiobookRunTime = AudiobookRunTime Integer
  deriving (Show, Eq)

audiobookRunTimeSchema :: FC.Fleece t => FC.Schema t AudiobookRunTime
audiobookRunTimeSchema =
  FC.coerceSchema FC.integer