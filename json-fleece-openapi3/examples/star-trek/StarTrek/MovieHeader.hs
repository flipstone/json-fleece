{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MovieHeader
  ( MovieHeader(..)
  , movieHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import StarTrek.MovieHeader.Title (Title, titleSchema)
import StarTrek.MovieHeader.Uid (Uid, uidSchema)

data MovieHeader = MovieHeader
  { uid :: Uid -- ^ Movie unique ID
  , title :: Title -- ^ Movie title
  }
  deriving (Eq, Show)

movieHeaderSchema :: FC.Fleece schema => schema MovieHeader
movieHeaderSchema =
  FC.object $
    FC.constructor MovieHeader
      #+ FC.required "uid" uid uidSchema
      #+ FC.required "title" title titleSchema