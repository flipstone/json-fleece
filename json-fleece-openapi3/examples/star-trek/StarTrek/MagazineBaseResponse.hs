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
      #+ FC.optionalField FC.OmitKey_DelegateNull "magazines" magazines (FC.list magazineBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "sort" sort responseSortSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "page" page responsePageSchema