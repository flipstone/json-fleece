{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.LiteratureBaseResponse
  ( LiteratureBaseResponse(..)
  , literatureBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.LiteratureBase as LiteratureBase
import qualified StarTrek.Types.ResponsePage as ResponsePage
import qualified StarTrek.Types.ResponseSort as ResponseSort

data LiteratureBaseResponse = LiteratureBaseResponse
  { page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  , literature :: Maybe [LiteratureBase.LiteratureBase] -- ^ Base literature, returned in search results
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  }
  deriving (Eq, Show)

literatureBaseResponseSchema :: FC.Fleece schema => schema LiteratureBaseResponse
literatureBaseResponseSchema =
  FC.object $
    FC.constructor LiteratureBaseResponse
      #+ FC.optional "page" page ResponsePage.responsePageSchema
      #+ FC.optional "literature" literature (FC.list LiteratureBase.literatureBaseSchema)
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema