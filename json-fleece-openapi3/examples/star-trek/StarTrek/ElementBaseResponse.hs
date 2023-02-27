{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ElementBaseResponse
  ( ElementBaseResponse(..)
  , elementBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ElementBase (ElementBase, elementBaseSchema)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)

data ElementBaseResponse = ElementBaseResponse
  { sort :: Maybe ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage -- ^ Object describing response page
  , elements :: Maybe [ElementBase] -- ^ Base element, returned in search results
  }
  deriving (Eq, Show)

elementBaseResponseSchema :: FC.Fleece schema => schema ElementBaseResponse
elementBaseResponseSchema =
  FC.object $
    FC.constructor ElementBaseResponse
      #+ FC.optional "sort" sort responseSortSchema
      #+ FC.optional "page" page responsePageSchema
      #+ FC.optional "elements" elements (FC.list elementBaseSchema)