{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.PerformerFull.VoicePerformer
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