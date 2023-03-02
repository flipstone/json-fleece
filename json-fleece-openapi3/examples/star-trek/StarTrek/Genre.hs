{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Genre
  ( Genre(..)
  , genreSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Genre.Name as Name
import qualified StarTrek.Genre.Uid as Uid

data Genre = Genre
  { name :: Maybe Name.Name -- ^ Genre name
  , uid :: Maybe Uid.Uid -- ^ Genre unique ID
  }
  deriving (Eq, Show)

genreSchema :: FC.Fleece schema => schema Genre
genreSchema =
  FC.object $
    FC.constructor Genre
      #+ FC.optional "name" name Name.nameSchema
      #+ FC.optional "uid" uid Uid.uidSchema