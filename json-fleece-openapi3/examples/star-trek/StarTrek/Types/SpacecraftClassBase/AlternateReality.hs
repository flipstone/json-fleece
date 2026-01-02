{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SpacecraftClassBase.AlternateReality
  ( AlternateReality(..)
  , alternateRealitySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype AlternateReality = AlternateReality Bool
  deriving (Show, Eq)

alternateRealitySchema :: FC.Fleece t => FC.Schema t AlternateReality
alternateRealitySchema =
  FC.coerceSchema FC.boolean