{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Genre
  ( Genre(..)
  , genreSchema
  ) where

import Data.Text (Text)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)

data Genre = Genre
  { name :: Maybe Text -- ^ Genre name
  , uid :: Maybe Text -- ^ Genre unique ID
  }
  deriving (Eq, Show)

genreSchema :: FC.Fleece schema => schema Genre
genreSchema =
  FC.object $
    FC.constructor Genre
      #+ FC.optional "name" name FC.text
      #+ FC.optional "uid" uid FC.text