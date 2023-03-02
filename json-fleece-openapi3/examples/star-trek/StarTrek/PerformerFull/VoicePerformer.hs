{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.PerformerFull.VoicePerformer
  ( VoicePerformer(..)
  , voicePerformerSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype VoicePerformer = VoicePerformer Bool
  deriving (Show, Eq)

voicePerformerSchema :: FC.Fleece schema => schema VoicePerformer
voicePerformerSchema =
  FC.coerceSchema FC.boolean