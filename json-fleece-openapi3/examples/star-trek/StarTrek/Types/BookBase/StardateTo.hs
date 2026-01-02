{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.BookBase.StardateTo
  ( StardateTo(..)
  , stardateToSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Float, Show)

newtype StardateTo = StardateTo Float
  deriving (Show, Eq)

stardateToSchema :: FC.Fleece t => FC.Schema t StardateTo
stardateToSchema =
  FC.coerceSchema FC.float