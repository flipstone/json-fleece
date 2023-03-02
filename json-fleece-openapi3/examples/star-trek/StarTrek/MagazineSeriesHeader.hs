{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MagazineSeriesHeader
  ( MagazineSeriesHeader(..)
  , magazineSeriesHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.MagazineSeriesHeader.Title as Title
import qualified StarTrek.MagazineSeriesHeader.Uid as Uid

data MagazineSeriesHeader = MagazineSeriesHeader
  { uid :: Uid.Uid -- ^ Magazine series unique ID
  , title :: Title.Title -- ^ Magazine series title
  }
  deriving (Eq, Show)

magazineSeriesHeaderSchema :: FC.Fleece schema => schema MagazineSeriesHeader
magazineSeriesHeaderSchema =
  FC.object $
    FC.constructor MagazineSeriesHeader
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.required "title" title Title.titleSchema