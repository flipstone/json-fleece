{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.BookBase.Audiobook
  ( Audiobook(..)
  , audiobookSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Audiobook = Audiobook Bool
  deriving (Show, Eq)

audiobookSchema :: FC.Fleece t => FC.Schema t Audiobook
audiobookSchema =
  FC.coerceSchema FC.boolean