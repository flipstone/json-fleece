{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.LiteratureBaseResponse
  ( LiteratureBaseResponse(..)
  , literatureBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.LiteratureBase as LiteratureBase
import qualified StarTrek.ResponsePage as ResponsePage
import qualified StarTrek.ResponseSort as ResponseSort

data LiteratureBaseResponse = LiteratureBaseResponse
  { sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  , literature :: Maybe [LiteratureBase.LiteratureBase] -- ^ Base literature, returned in search results
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

literatureBaseResponseSchema :: FC.Fleece schema => schema LiteratureBaseResponse
literatureBaseResponseSchema =
  FC.object $
    FC.constructor LiteratureBaseResponse
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema
      #+ FC.optional "literature" literature (FC.list LiteratureBase.literatureBaseSchema)
      #+ FC.optional "page" page ResponsePage.responsePageSchema