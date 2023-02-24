{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TechnologyHeader
  ( TechnologyHeader(..)
  , technologyHeaderSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Show)

data TechnologyHeader = TechnologyHeader
  { name :: Text -- ^ Technology name
  , uid :: Text -- ^ Technology unique ID
  }
  deriving (Eq, Show)

technologyHeaderSchema :: FC.Fleece schema => schema TechnologyHeader
technologyHeaderSchema =
  FC.object $
    FC.constructor TechnologyHeader
      #+ FC.required "name" name FC.text
      #+ FC.required "uid" uid FC.text