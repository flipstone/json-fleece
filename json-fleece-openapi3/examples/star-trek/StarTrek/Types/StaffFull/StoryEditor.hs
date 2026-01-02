{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffFull.StoryEditor
  ( StoryEditor(..)
  , storyEditorSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype StoryEditor = StoryEditor Bool
  deriving (Show, Eq)

storyEditorSchema :: FC.Fleece t => FC.Schema t StoryEditor
storyEditorSchema =
  FC.coerceSchema FC.boolean