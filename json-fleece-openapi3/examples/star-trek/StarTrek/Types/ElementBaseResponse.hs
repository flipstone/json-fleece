{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ElementBaseResponse
  ( ElementBaseResponse(..)
  , elementBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.ElementBase as ElementBase
import qualified StarTrek.Types.ResponsePage as ResponsePage
import qualified StarTrek.Types.ResponseSort as ResponseSort

data ElementBaseResponse = ElementBaseResponse
  { elements :: Maybe [ElementBase.ElementBase] -- ^ Base element, returned in search results
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  }
  deriving (Eq, Show)

elementBaseResponseSchema :: FC.Fleece t => FC.Schema t ElementBaseResponse
elementBaseResponseSchema =
  FC.object $
    FC.constructor ElementBaseResponse
      #+ FC.optional "elements" elements (FC.list ElementBase.elementBaseSchema)
      #+ FC.optional "page" page ResponsePage.responsePageSchema
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema