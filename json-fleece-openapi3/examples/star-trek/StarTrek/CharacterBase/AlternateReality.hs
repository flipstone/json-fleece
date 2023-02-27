{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CharacterBase.AlternateReality
  ( AlternateReality(..)
  , alternateRealitySchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype AlternateReality = AlternateReality Bool
  deriving (Show, Eq)

alternateRealitySchema :: FC.Fleece schema => schema AlternateReality
alternateRealitySchema =
  FC.coerceSchema FC.boolean