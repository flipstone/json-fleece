{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MagazineFullResponse
  ( MagazineFullResponse(..)
  , magazineFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.MagazineFull as MagazineFull

data MagazineFullResponse = MagazineFullResponse
  { magazine :: Maybe MagazineFull.MagazineFull -- ^ Full magazine, returned when queried using UID
  }
  deriving (Eq, Show)

magazineFullResponseSchema :: FC.Fleece schema => schema MagazineFullResponse
magazineFullResponseSchema =
  FC.object $
    FC.constructor MagazineFullResponse
      #+ FC.optional "magazine" magazine MagazineFull.magazineFullSchema