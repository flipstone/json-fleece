{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TechnologyBaseResponse
  ( TechnologyBaseResponse(..)
  , technologyBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.ResponsePage as ResponsePage
import qualified StarTrek.ResponseSort as ResponseSort
import qualified StarTrek.TechnologyBase as TechnologyBase

data TechnologyBaseResponse = TechnologyBaseResponse
  { technology :: Maybe [TechnologyBase.TechnologyBase] -- ^ Base technology, returned in search results
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

technologyBaseResponseSchema :: FC.Fleece schema => schema TechnologyBaseResponse
technologyBaseResponseSchema =
  FC.object $
    FC.constructor TechnologyBaseResponse
      #+ FC.optional "technology" technology (FC.list TechnologyBase.technologyBaseSchema)
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema
      #+ FC.optional "page" page ResponsePage.responsePageSchema