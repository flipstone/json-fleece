{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MaterialHeader
  ( MaterialHeader(..)
  , materialHeaderSchema
  ) where

import Data.Text (Text)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)

data MaterialHeader = MaterialHeader
  { name :: Text -- ^ Material name
  , uid :: Text -- ^ Material unique ID
  }
  deriving (Eq, Show)

materialHeaderSchema :: FC.Fleece schema => schema MaterialHeader
materialHeaderSchema =
  FC.object $
    FC.constructor MaterialHeader
      #+ FC.required "name" name FC.text
      #+ FC.required "uid" uid FC.text