{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ComicSeriesBase.StardateTo
  ( StardateTo(..)
  , stardateToSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Float, Show)

newtype StardateTo = StardateTo Float
  deriving (Show, Eq)

stardateToSchema :: FC.Fleece schema => schema StardateTo
stardateToSchema =
  FC.coerceSchema FC.float