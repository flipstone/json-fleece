{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TechnologyBaseResponse
  ( TechnologyBaseResponse(..)
  , technologyBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
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
      #+ FC.optional "technology" technology (FC.list technologyBaseSchema)
      #+ FC.optional "sort" sort responseSortSchema
      #+ FC.optional "page" page responsePageSchema