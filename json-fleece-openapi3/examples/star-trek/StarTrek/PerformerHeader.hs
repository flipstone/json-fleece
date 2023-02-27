{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.PerformerHeader
  ( PerformerHeader(..)
  , performerHeaderSchema
  ) where

import Data.Text (Text)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)

data PerformerHeader = PerformerHeader
  { name :: Text -- ^ Performer name
  , uid :: Text -- ^ Performer unique ID
  }
  deriving (Eq, Show)

performerHeaderSchema :: FC.Fleece schema => schema PerformerHeader
performerHeaderSchema =
  FC.object $
    FC.constructor PerformerHeader
      #+ FC.required "name" name FC.text
      #+ FC.required "uid" uid FC.text