{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CharacterBase.HologramActivationDate
  ( HologramActivationDate(..)
  , hologramActivationDateSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype HologramActivationDate = HologramActivationDate Text
  deriving (Show, Eq)

hologramActivationDateSchema :: FC.Fleece schema => schema HologramActivationDate
hologramActivationDateSchema =
  FC.coerceSchema FC.text