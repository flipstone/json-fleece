{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Genre
  ( Genre(..)
  , genreSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.Genre.Name (Name, nameSchema)
import StarTrek.Genre.Uid (Uid, uidSchema)

data Genre = Genre
  { name :: Maybe Name -- ^ Genre name
  , uid :: Maybe Uid -- ^ Genre unique ID
  }
  deriving (Eq, Show)

genreSchema :: FC.Fleece schema => schema Genre
genreSchema =
  FC.object $
    FC.constructor Genre
      #+ FC.optional "name" name nameSchema
      #+ FC.optional "uid" uid uidSchema