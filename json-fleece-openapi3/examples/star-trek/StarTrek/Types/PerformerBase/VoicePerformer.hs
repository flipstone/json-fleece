{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.PerformerBase.VoicePerformer
  ( VoicePerformer(..)
  , voicePerformerSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype VoicePerformer = VoicePerformer Bool
  deriving (Show, Eq)

voicePerformerSchema :: FC.Fleece t => FC.Schema t VoicePerformer
voicePerformerSchema =
  FC.coerceSchema FC.boolean