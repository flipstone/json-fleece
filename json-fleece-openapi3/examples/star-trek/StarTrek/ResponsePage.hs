{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ResponsePage
  ( ResponsePage(..)
  , responsePageSchema
  ) where

import Data.Int (Int32)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Bool, Eq, Maybe, Show)

data ResponsePage = ResponsePage
  { totalElements :: Maybe Int32 -- ^ Total elements found
  , numberOfElements :: Maybe Int32 -- ^ Number of elements in page
  , pageNumber :: Maybe Int32 -- ^ Zero-based page number
  , totalPages :: Maybe Int32 -- ^ Total pages found
  , pageSize :: Maybe Int32 -- ^ Page size
  , firstPage :: Maybe Bool -- ^ Whether it is the first page
  , lastPage :: Maybe Bool -- ^ Whether it is the last page
  }
  deriving (Eq, Show)

responsePageSchema :: FC.Fleece schema => schema ResponsePage
responsePageSchema =
  FC.object $
    FC.constructor ResponsePage
      #+ FC.optional "totalElements" totalElements FC.int32
      #+ FC.optional "numberOfElements" numberOfElements FC.int32
      #+ FC.optional "pageNumber" pageNumber FC.int32
      #+ FC.optional "totalPages" totalPages FC.int32
      #+ FC.optional "pageSize" pageSize FC.int32
      #+ FC.optional "firstPage" firstPage FC.boolean
      #+ FC.optional "lastPage" lastPage FC.boolean