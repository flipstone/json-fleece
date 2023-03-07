{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.AstronomicalObjectBaseResponse
  ( AstronomicalObjectBaseResponse(..)
  , astronomicalObjectBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.AstronomicalObjectBase as AstronomicalObjectBase
import qualified StarTrek.Types.ResponsePage as ResponsePage
import qualified StarTrek.Types.ResponseSort as ResponseSort

data AstronomicalObjectBaseResponse = AstronomicalObjectBaseResponse
  { sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  , astronomicalObjects :: Maybe [AstronomicalObjectBase.AstronomicalObjectBase] -- ^ Base astronomical object, returned in search results
  }
  deriving (Eq, Show)

astronomicalObjectBaseResponseSchema :: FC.Fleece schema => schema AstronomicalObjectBaseResponse
astronomicalObjectBaseResponseSchema =
  FC.object $
    FC.constructor AstronomicalObjectBaseResponse
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema
      #+ FC.optional "page" page ResponsePage.responsePageSchema
      #+ FC.optional "astronomicalObjects" astronomicalObjects (FC.list AstronomicalObjectBase.astronomicalObjectBaseSchema)