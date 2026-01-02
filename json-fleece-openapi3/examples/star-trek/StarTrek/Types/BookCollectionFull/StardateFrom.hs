{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.BookCollectionFull.StardateFrom
  ( StardateFrom(..)
  , stardateFromSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Float, Show)

newtype StardateFrom = StardateFrom Float
  deriving (Show, Eq)

stardateFromSchema :: FC.Fleece t => FC.Schema t StardateFrom
stardateFromSchema =
  FC.coerceSchema FC.float