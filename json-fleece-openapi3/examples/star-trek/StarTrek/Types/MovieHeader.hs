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
  { title :: Title.Title -- ^ Movie title
  , uid :: Uid.Uid -- ^ Movie unique ID
  }
  deriving (Eq, Show)

movieHeaderSchema :: FC.Fleece t => FC.Schema t MovieHeader
movieHeaderSchema =
  FC.object $
    FC.constructor MovieHeader
      #+ FC.required "title" title Title.titleSchema
      #+ FC.required "uid" uid Uid.uidSchema