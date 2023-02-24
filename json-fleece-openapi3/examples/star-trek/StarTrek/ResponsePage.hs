{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ResponsePage
  ( ResponsePage(..)
  , responsePageSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Bool, Eq, Integer, Maybe, Show)

data ResponsePage = ResponsePage
  { totalElements :: Maybe Integer -- ^ Total elements found
  , numberOfElements :: Maybe Integer -- ^ Number of elements in page
  , pageNumber :: Maybe Integer -- ^ Zero-based page number
  , totalPages :: Maybe Integer -- ^ Total pages found
  , pageSize :: Maybe Integer -- ^ Page size
  , firstPage :: Maybe Bool -- ^ Whether it is the first page
  , lastPage :: Maybe Bool -- ^ Whether it is the last page
  }
  deriving (Eq, Show)

responsePageSchema :: FC.Fleece schema => schema ResponsePage
responsePageSchema =
  FC.object $
    FC.constructor ResponsePage
      #+ FC.optionalField FC.OmitKey_DelegateNull "totalElements" totalElements FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "numberOfElements" numberOfElements FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "pageNumber" pageNumber FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "totalPages" totalPages FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "pageSize" pageSize FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "firstPage" firstPage FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "lastPage" lastPage FC.boolean