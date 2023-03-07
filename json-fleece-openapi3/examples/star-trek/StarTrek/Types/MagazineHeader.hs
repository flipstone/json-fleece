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
  { uid :: Uid.Uid -- ^ Magazine unique ID
  , title :: Title.Title -- ^ Magazine title
  }
  deriving (Eq, Show)

magazineHeaderSchema :: FC.Fleece schema => schema MagazineHeader
magazineHeaderSchema =
  FC.object $
    FC.constructor MagazineHeader
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.required "title" title Title.titleSchema