{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MovieHeader
  ( MovieHeader(..)
  , movieHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.Types.MovieHeader.Title as Title
import qualified StarTrek.Types.MovieHeader.Uid as Uid

data MovieHeader = MovieHeader
  { uid :: Uid.Uid -- ^ Movie unique ID
  , title :: Title.Title -- ^ Movie title
  }
  deriving (Eq, Show)

movieHeaderSchema :: FC.Fleece schema => schema MovieHeader
movieHeaderSchema =
  FC.object $
    FC.constructor MovieHeader
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.required "title" title Title.titleSchema