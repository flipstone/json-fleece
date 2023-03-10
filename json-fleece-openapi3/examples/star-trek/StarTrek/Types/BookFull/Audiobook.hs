{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.BookFull.Audiobook
  ( Audiobook(..)
  , audiobookSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Audiobook = Audiobook Bool
  deriving (Show, Eq)

audiobookSchema :: FC.Fleece schema => schema Audiobook
audiobookSchema =
  FC.coerceSchema FC.boolean