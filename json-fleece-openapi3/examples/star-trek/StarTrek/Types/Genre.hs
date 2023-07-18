{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.Genre
  ( Genre(..)
  , genreSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.Genre.Name as Name
import qualified StarTrek.Types.Genre.Uid as Uid

data Genre = Genre
  { uid :: Maybe Uid.Uid -- ^ Genre unique ID
  , name :: Maybe Name.Name -- ^ Genre name
  }
  deriving (Eq, Show)

genreSchema :: FC.Fleece schema => schema Genre
genreSchema =
  FC.object $
    FC.constructor Genre
      #+ FC.optional "uid" uid Uid.uidSchema
      #+ FC.optional "name" name Name.nameSchema