{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SpacecraftClassBase.NumberOfDecks
  ( NumberOfDecks(..)
  , numberOfDecksSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype NumberOfDecks = NumberOfDecks Integer
  deriving (Show, Eq)

numberOfDecksSchema :: FC.Fleece schema => schema NumberOfDecks
numberOfDecksSchema =
  FC.coerceSchema FC.integer