{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SpacecraftClassFull.NumberOfDecks
  ( NumberOfDecks(..)
  , numberOfDecksSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype NumberOfDecks = NumberOfDecks Integer
  deriving (Show, Eq)

numberOfDecksSchema :: FC.Fleece t => FC.Schema t NumberOfDecks
numberOfDecksSchema =
  FC.coerceSchema FC.integer