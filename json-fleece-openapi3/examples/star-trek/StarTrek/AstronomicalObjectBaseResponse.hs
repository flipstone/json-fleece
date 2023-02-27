{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.AstronomicalObjectBaseResponse
  ( AstronomicalObjectBaseResponse(..)
  , astronomicalObjectBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.AstronomicalObjectBase (AstronomicalObjectBase, astronomicalObjectBaseSchema)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)

data AstronomicalObjectBaseResponse = AstronomicalObjectBaseResponse
  { sort :: Maybe ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage -- ^ Object describing response page
  , astronomicalObjects :: Maybe [AstronomicalObjectBase] -- ^ Base astronomical object, returned in search results
  }
  deriving (Eq, Show)

astronomicalObjectBaseResponseSchema :: FC.Fleece schema => schema AstronomicalObjectBaseResponse
astronomicalObjectBaseResponseSchema =
  FC.object $
    FC.constructor AstronomicalObjectBaseResponse
      #+ FC.optional "sort" sort responseSortSchema
      #+ FC.optional "page" page responsePageSchema
      #+ FC.optional "astronomicalObjects" astronomicalObjects (FC.list astronomicalObjectBaseSchema)