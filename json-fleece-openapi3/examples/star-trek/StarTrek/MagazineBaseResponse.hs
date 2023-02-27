{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MagazineBaseResponse
  ( MagazineBaseResponse(..)
  , magazineBaseResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.MagazineBase (MagazineBase, magazineBaseSchema)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)

data MagazineBaseResponse = MagazineBaseResponse
  { magazines :: Maybe [MagazineBase] -- ^ List of magazines matching given criteria
  , sort :: Maybe ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

magazineBaseResponseSchema :: FC.Fleece schema => schema MagazineBaseResponse
magazineBaseResponseSchema =
  FC.object $
    FC.constructor MagazineBaseResponse
      #+ FC.optional "magazines" magazines (FC.list magazineBaseSchema)
      #+ FC.optional "sort" sort responseSortSchema
      #+ FC.optional "page" page responsePageSchema