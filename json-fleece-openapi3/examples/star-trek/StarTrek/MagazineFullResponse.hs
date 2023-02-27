{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MagazineFullResponse
  ( MagazineFullResponse(..)
  , magazineFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.MagazineFull (MagazineFull, magazineFullSchema)

data MagazineFullResponse = MagazineFullResponse
  { magazine :: Maybe MagazineFull -- ^ Full magazine, returned when queried using UID
  }
  deriving (Eq, Show)

magazineFullResponseSchema :: FC.Fleece schema => schema MagazineFullResponse
magazineFullResponseSchema =
  FC.object $
    FC.constructor MagazineFullResponse
      #+ FC.optional "magazine" magazine magazineFullSchema