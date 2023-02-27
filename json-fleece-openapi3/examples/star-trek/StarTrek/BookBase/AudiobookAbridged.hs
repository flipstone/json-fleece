{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookBase.AudiobookAbridged
  ( AudiobookAbridged(..)
  , audiobookAbridgedSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype AudiobookAbridged = AudiobookAbridged Bool
  deriving (Show, Eq)

audiobookAbridgedSchema :: FC.Fleece schema => schema AudiobookAbridged
audiobookAbridgedSchema =
  FC.coerceSchema FC.boolean