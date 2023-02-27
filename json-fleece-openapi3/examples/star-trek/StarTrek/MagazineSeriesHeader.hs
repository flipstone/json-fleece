{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MagazineSeriesHeader
  ( MagazineSeriesHeader(..)
  , magazineSeriesHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import StarTrek.MagazineSeriesHeader.Title (Title, titleSchema)
import StarTrek.MagazineSeriesHeader.Uid (Uid, uidSchema)

data MagazineSeriesHeader = MagazineSeriesHeader
  { uid :: Uid -- ^ Magazine series unique ID
  , title :: Title -- ^ Magazine series title
  }
  deriving (Eq, Show)

magazineSeriesHeaderSchema :: FC.Fleece schema => schema MagazineSeriesHeader
magazineSeriesHeaderSchema =
  FC.object $
    FC.constructor MagazineSeriesHeader
      #+ FC.required "uid" uid uidSchema
      #+ FC.required "title" title titleSchema