{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MagazineHeader
  ( MagazineHeader(..)
  , magazineHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import StarTrek.MagazineHeader.Title (Title, titleSchema)
import StarTrek.MagazineHeader.Uid (Uid, uidSchema)

data MagazineHeader = MagazineHeader
  { uid :: Uid -- ^ Magazine unique ID
  , title :: Title -- ^ Magazine title
  }
  deriving (Eq, Show)

magazineHeaderSchema :: FC.Fleece schema => schema MagazineHeader
magazineHeaderSchema =
  FC.object $
    FC.constructor MagazineHeader
      #+ FC.required "uid" uid uidSchema
      #+ FC.required "title" title titleSchema