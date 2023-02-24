{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TechnologyBaseResponse
  ( TechnologyBaseResponse(..)
  , technologyBaseResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)
import StarTrek.TechnologyBase (TechnologyBase, technologyBaseSchema)

data TechnologyBaseResponse = TechnologyBaseResponse
  { technology :: Maybe [TechnologyBase] -- ^ List of technology matching given criteria
  , sort :: Maybe ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

technologyBaseResponseSchema :: FC.Fleece schema => schema TechnologyBaseResponse
technologyBaseResponseSchema =
  FC.object $
    FC.constructor TechnologyBaseResponse
      #+ FC.optionalField FC.OmitKey_DelegateNull "technology" technology (FC.list technologyBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "sort" sort responseSortSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "page" page responsePageSchema