{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CharacterBase.HologramActivationDate
  ( HologramActivationDate(..)
  , hologramActivationDateSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype HologramActivationDate = HologramActivationDate T.Text
  deriving (Show, Eq)

hologramActivationDateSchema :: FC.Fleece schema => schema HologramActivationDate
hologramActivationDateSchema =
  FC.coerceSchema FC.text