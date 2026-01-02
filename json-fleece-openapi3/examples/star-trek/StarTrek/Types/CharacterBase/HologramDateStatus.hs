{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.CharacterBase.HologramDateStatus
  ( HologramDateStatus(..)
  , hologramDateStatusSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype HologramDateStatus = HologramDateStatus T.Text
  deriving (Show, Eq)

hologramDateStatusSchema :: FC.Fleece t => FC.Schema t HologramDateStatus
hologramDateStatusSchema =
  FC.coerceSchema FC.text