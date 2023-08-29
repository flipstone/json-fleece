{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MagazineHeader
  ( MagazineHeader(..)
  , magazineHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.Types.MagazineHeader.Title as Title
import qualified StarTrek.Types.MagazineHeader.Uid as Uid

data MagazineHeader = MagazineHeader
  { title :: Title.Title -- ^ Magazine title
  , uid :: Uid.Uid -- ^ Magazine unique ID
  }
  deriving (Eq, Show)

magazineHeaderSchema :: FC.Fleece schema => schema MagazineHeader
magazineHeaderSchema =
  FC.object $
    FC.constructor MagazineHeader
      #+ FC.required "title" title Title.titleSchema
      #+ FC.required "uid" uid Uid.uidSchema