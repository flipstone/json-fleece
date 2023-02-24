{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MagazineHeader
  ( MagazineHeader(..)
  , magazineHeaderSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Show)

data MagazineHeader = MagazineHeader
  { uid :: Text -- ^ Magazine unique ID
  , title :: Text -- ^ Magazine title
  }
  deriving (Eq, Show)

magazineHeaderSchema :: FC.Fleece schema => schema MagazineHeader
magazineHeaderSchema =
  FC.object $
    FC.constructor MagazineHeader
      #+ FC.required "uid" uid FC.text
      #+ FC.required "title" title FC.text