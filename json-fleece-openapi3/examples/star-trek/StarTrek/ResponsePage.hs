{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ResponsePage
  ( ResponsePage(..)
  , responsePageSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ResponsePage.FirstPage (FirstPage, firstPageSchema)
import StarTrek.ResponsePage.LastPage (LastPage, lastPageSchema)
import StarTrek.ResponsePage.NumberOfElements (NumberOfElements, numberOfElementsSchema)
import StarTrek.ResponsePage.PageNumber (PageNumber, pageNumberSchema)
import StarTrek.ResponsePage.PageSize (PageSize, pageSizeSchema)
import StarTrek.ResponsePage.TotalElements (TotalElements, totalElementsSchema)
import StarTrek.ResponsePage.TotalPages (TotalPages, totalPagesSchema)

data ResponsePage = ResponsePage
  { totalElements :: Maybe TotalElements -- ^ Total elements found
  , numberOfElements :: Maybe NumberOfElements -- ^ Number of elements in page
  , pageNumber :: Maybe PageNumber -- ^ Zero-based page number
  , totalPages :: Maybe TotalPages -- ^ Total pages found
  , pageSize :: Maybe PageSize -- ^ Page size
  , firstPage :: Maybe FirstPage -- ^ Whether it is the first page
  , lastPage :: Maybe LastPage -- ^ Whether it is the last page
  }
  deriving (Eq, Show)

responsePageSchema :: FC.Fleece schema => schema ResponsePage
responsePageSchema =
  FC.object $
    FC.constructor ResponsePage
      #+ FC.optional "totalElements" totalElements totalElementsSchema
      #+ FC.optional "numberOfElements" numberOfElements numberOfElementsSchema
      #+ FC.optional "pageNumber" pageNumber pageNumberSchema
      #+ FC.optional "totalPages" totalPages totalPagesSchema
      #+ FC.optional "pageSize" pageSize pageSizeSchema
      #+ FC.optional "firstPage" firstPage firstPageSchema
      #+ FC.optional "lastPage" lastPage lastPageSchema