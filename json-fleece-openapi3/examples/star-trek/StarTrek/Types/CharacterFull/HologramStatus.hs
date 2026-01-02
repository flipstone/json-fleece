{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.CharacterFull.HologramStatus
  ( HologramStatus(..)
  , hologramStatusSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype HologramStatus = HologramStatus T.Text
  deriving (Show, Eq)

hologramStatusSchema :: FC.Fleece t => FC.Schema t HologramStatus
hologramStatusSchema =
  FC.coerceSchema FC.text