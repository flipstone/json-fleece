{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.OccupationHeader
  ( OccupationHeader(..)
  , occupationHeaderSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Show)

data OccupationHeader = OccupationHeader
  { name :: Text -- ^ Occupation name
  , uid :: Text -- ^ Occupation unique ID
  }
  deriving (Eq, Show)

occupationHeaderSchema :: FC.Fleece schema => schema OccupationHeader
occupationHeaderSchema =
  FC.object $
    FC.constructor OccupationHeader
      #+ FC.required "name" name FC.text
      #+ FC.required "uid" uid FC.text