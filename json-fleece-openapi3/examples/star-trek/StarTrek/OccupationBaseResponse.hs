{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.OccupationBaseResponse
  ( OccupationBaseResponse(..)
  , occupationBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.OccupationBase (OccupationBase, occupationBaseSchema)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)

data OccupationBaseResponse = OccupationBaseResponse
  { occupations :: Maybe [OccupationBase] -- ^ Base occupations, returned in search results
  , sort :: Maybe ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

occupationBaseResponseSchema :: FC.Fleece schema => schema OccupationBaseResponse
occupationBaseResponseSchema =
  FC.object $
    FC.constructor OccupationBaseResponse
      #+ FC.optional "occupations" occupations (FC.list occupationBaseSchema)
      #+ FC.optional "sort" sort responseSortSchema
      #+ FC.optional "page" page responsePageSchema