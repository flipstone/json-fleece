{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.AstronomicalObjectBaseResponse
  ( AstronomicalObjectBaseResponse(..)
  , astronomicalObjectBaseResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.AstronomicalObjectBase (AstronomicalObjectBase, astronomicalObjectBaseSchema)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)

data AstronomicalObjectBaseResponse = AstronomicalObjectBaseResponse
  { sort :: Maybe ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage -- ^ Object describing response page
  , astronomicalObjects :: Maybe [AstronomicalObjectBase] -- ^ List of astronomical objects matching given criteria
  }
  deriving (Eq, Show)

astronomicalObjectBaseResponseSchema :: FC.Fleece schema => schema AstronomicalObjectBaseResponse
astronomicalObjectBaseResponseSchema =
  FC.object $
    FC.constructor AstronomicalObjectBaseResponse
      #+ FC.optional "sort" sort responseSortSchema
      #+ FC.optional "page" page responsePageSchema
      #+ FC.optional "astronomicalObjects" astronomicalObjects (FC.list astronomicalObjectBaseSchema)