{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MagazineBaseResponse
  ( MagazineBaseResponse(..)
  , magazineBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.MagazineBase as MagazineBase
import qualified StarTrek.Types.ResponsePage as ResponsePage
import qualified StarTrek.Types.ResponseSort as ResponseSort

data MagazineBaseResponse = MagazineBaseResponse
  { magazines :: Maybe [MagazineBase.MagazineBase] -- ^ Base magazine, returned in search results
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

magazineBaseResponseSchema :: FC.Fleece schema => schema MagazineBaseResponse
magazineBaseResponseSchema =
  FC.object $
    FC.constructor MagazineBaseResponse
      #+ FC.optional "magazines" magazines (FC.list MagazineBase.magazineBaseSchema)
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema
      #+ FC.optional "page" page ResponsePage.responsePageSchema