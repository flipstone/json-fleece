{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ElementHeader
  ( ElementHeader(..)
  , elementHeaderSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Show)

data ElementHeader = ElementHeader
  { name :: Text -- ^ Element name
  , uid :: Text -- ^ Element unique ID
  }
  deriving (Eq, Show)

elementHeaderSchema :: FC.Fleece schema => schema ElementHeader
elementHeaderSchema =
  FC.object $
    FC.constructor ElementHeader
      #+ FC.required "name" name FC.text
      #+ FC.required "uid" uid FC.text