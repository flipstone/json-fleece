{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MagazineSeriesHeader
  ( MagazineSeriesHeader(..)
  , magazineSeriesHeaderSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Show)

data MagazineSeriesHeader = MagazineSeriesHeader
  { uid :: Text -- ^ Magazine series unique ID
  , title :: Text -- ^ Magazine series title
  }
  deriving (Eq, Show)

magazineSeriesHeaderSchema :: FC.Fleece schema => schema MagazineSeriesHeader
magazineSeriesHeaderSchema =
  FC.object $
    FC.constructor MagazineSeriesHeader
      #+ FC.required "uid" uid FC.text
      #+ FC.required "title" title FC.text