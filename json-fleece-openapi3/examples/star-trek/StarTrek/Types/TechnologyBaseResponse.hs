{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TechnologyBaseResponse
  ( TechnologyBaseResponse(..)
  , technologyBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.ResponsePage as ResponsePage
import qualified StarTrek.Types.ResponseSort as ResponseSort
import qualified StarTrek.Types.TechnologyBase as TechnologyBase

data TechnologyBaseResponse = TechnologyBaseResponse
  { page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  , technology :: Maybe [TechnologyBase.TechnologyBase] -- ^ Base technology, returned in search results
  }
  deriving (Eq, Show)

technologyBaseResponseSchema :: FC.Fleece t => FC.Schema t TechnologyBaseResponse
technologyBaseResponseSchema =
  FC.object $
    FC.constructor TechnologyBaseResponse
      #+ FC.optional "page" page ResponsePage.responsePageSchema
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema
      #+ FC.optional "technology" technology (FC.list TechnologyBase.technologyBaseSchema)