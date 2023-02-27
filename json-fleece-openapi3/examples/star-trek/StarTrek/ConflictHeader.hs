{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ConflictHeader
  ( ConflictHeader(..)
  , conflictHeaderSchema
  ) where

import Data.Text (Text)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)

data ConflictHeader = ConflictHeader
  { name :: Text -- ^ Conflict name
  , uid :: Text -- ^ Conflict unique ID
  }
  deriving (Eq, Show)

conflictHeaderSchema :: FC.Fleece schema => schema ConflictHeader
conflictHeaderSchema =
  FC.object $
    FC.constructor ConflictHeader
      #+ FC.required "name" name FC.text
      #+ FC.required "uid" uid FC.text