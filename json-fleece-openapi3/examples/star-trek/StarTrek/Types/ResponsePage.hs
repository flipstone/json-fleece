{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ResponsePage
  ( ResponsePage(..)
  , responsePageSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.ResponsePage.FirstPage as FirstPage
import qualified StarTrek.Types.ResponsePage.LastPage as LastPage
import qualified StarTrek.Types.ResponsePage.NumberOfElements as NumberOfElements
import qualified StarTrek.Types.ResponsePage.PageNumber as PageNumber
import qualified StarTrek.Types.ResponsePage.PageSize as PageSize
import qualified StarTrek.Types.ResponsePage.TotalElements as TotalElements
import qualified StarTrek.Types.ResponsePage.TotalPages as TotalPages

data ResponsePage = ResponsePage
  { firstPage :: Maybe FirstPage.FirstPage -- ^ Whether it is the first page
  , lastPage :: Maybe LastPage.LastPage -- ^ Whether it is the last page
  , numberOfElements :: Maybe NumberOfElements.NumberOfElements -- ^ Number of elements in page
  , pageNumber :: Maybe PageNumber.PageNumber -- ^ Zero-based page number
  , pageSize :: Maybe PageSize.PageSize -- ^ Page size
  , totalElements :: Maybe TotalElements.TotalElements -- ^ Total elements found
  , totalPages :: Maybe TotalPages.TotalPages -- ^ Total pages found
  }
  deriving (Eq, Show)

responsePageSchema :: FC.Fleece t => FC.Schema t ResponsePage
responsePageSchema =
  FC.object $
    FC.constructor ResponsePage
      #+ FC.optional "firstPage" firstPage FirstPage.firstPageSchema
      #+ FC.optional "lastPage" lastPage LastPage.lastPageSchema
      #+ FC.optional "numberOfElements" numberOfElements NumberOfElements.numberOfElementsSchema
      #+ FC.optional "pageNumber" pageNumber PageNumber.pageNumberSchema
      #+ FC.optional "pageSize" pageSize PageSize.pageSizeSchema
      #+ FC.optional "totalElements" totalElements TotalElements.totalElementsSchema
      #+ FC.optional "totalPages" totalPages TotalPages.totalPagesSchema