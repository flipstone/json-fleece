{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpacecraftClassBase.NumberOfDecks
  ( NumberOfDecks(..)
  , numberOfDecksSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype NumberOfDecks = NumberOfDecks Integer
  deriving (Show, Eq)

numberOfDecksSchema :: FC.Fleece schema => schema NumberOfDecks
numberOfDecksSchema =
  FC.coerceSchema FC.integer