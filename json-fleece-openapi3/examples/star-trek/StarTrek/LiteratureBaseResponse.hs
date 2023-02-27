{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.LiteratureBaseResponse
  ( LiteratureBaseResponse(..)
  , literatureBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.LiteratureBase (LiteratureBase, literatureBaseSchema)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)

data LiteratureBaseResponse = LiteratureBaseResponse
  { sort :: Maybe ResponseSort -- ^ Response sort
  , literature :: Maybe [LiteratureBase] -- ^ List of literature matching given criteria
  , page :: Maybe ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

literatureBaseResponseSchema :: FC.Fleece schema => schema LiteratureBaseResponse
literatureBaseResponseSchema =
  FC.object $
    FC.constructor LiteratureBaseResponse
      #+ FC.optional "sort" sort responseSortSchema
      #+ FC.optional "literature" literature (FC.list literatureBaseSchema)
      #+ FC.optional "page" page responsePageSchema