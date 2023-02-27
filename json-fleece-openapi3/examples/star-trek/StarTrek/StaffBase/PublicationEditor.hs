{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffBase.PublicationEditor
  ( PublicationEditor(..)
  , publicationEditorSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype PublicationEditor = PublicationEditor Bool
  deriving (Show, Eq)

publicationEditorSchema :: FC.Fleece schema => schema PublicationEditor
publicationEditorSchema =
  FC.coerceSchema FC.boolean