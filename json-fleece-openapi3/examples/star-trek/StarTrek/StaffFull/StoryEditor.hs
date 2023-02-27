{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffFull.StoryEditor
  ( StoryEditor(..)
  , storyEditorSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype StoryEditor = StoryEditor Bool
  deriving (Show, Eq)

storyEditorSchema :: FC.Fleece schema => schema StoryEditor
storyEditorSchema =
  FC.coerceSchema FC.boolean