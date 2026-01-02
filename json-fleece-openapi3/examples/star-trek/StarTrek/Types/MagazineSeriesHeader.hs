{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MagazineSeriesHeader
  ( MagazineSeriesHeader(..)
  , magazineSeriesHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.Types.MagazineSeriesHeader.Title as Title
import qualified StarTrek.Types.MagazineSeriesHeader.Uid as Uid

data MagazineSeriesHeader = MagazineSeriesHeader
  { title :: Title.Title -- ^ Magazine series title
  , uid :: Uid.Uid -- ^ Magazine series unique ID
  }
  deriving (Eq, Show)

magazineSeriesHeaderSchema :: FC.Fleece t => FC.Schema t MagazineSeriesHeader
magazineSeriesHeaderSchema =
  FC.object $
    FC.constructor MagazineSeriesHeader
      #+ FC.required "title" title Title.titleSchema
      #+ FC.required "uid" uid Uid.uidSchema