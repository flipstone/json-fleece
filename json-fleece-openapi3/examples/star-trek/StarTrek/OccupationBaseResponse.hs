{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.OccupationBaseResponse
  ( OccupationBaseResponse(..)
  , occupationBaseResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.OccupationBase (OccupationBase, occupationBaseSchema)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)

data OccupationBaseResponse = OccupationBaseResponse
  { occupations :: Maybe [OccupationBase] -- ^ List of occupations matching given criteria
  , sort :: Maybe ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

occupationBaseResponseSchema :: FC.Fleece schema => schema OccupationBaseResponse
occupationBaseResponseSchema =
  FC.object $
    FC.constructor OccupationBaseResponse
      #+ FC.optionalField FC.OmitKey_DelegateNull "occupations" occupations (FC.list occupationBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "sort" sort responseSortSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "page" page responsePageSchema