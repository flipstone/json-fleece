{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffBase.PublicationEditor
  ( PublicationEditor(..)
  , publicationEditorSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype PublicationEditor = PublicationEditor Bool
  deriving (Show, Eq)

publicationEditorSchema :: FC.Fleece t => FC.Schema t PublicationEditor
publicationEditorSchema =
  FC.coerceSchema FC.boolean