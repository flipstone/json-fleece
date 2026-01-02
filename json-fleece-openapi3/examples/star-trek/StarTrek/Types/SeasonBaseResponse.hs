{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SeasonBaseResponse
  ( SeasonBaseResponse(..)
  , seasonBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.ResponsePage as ResponsePage
import qualified StarTrek.Types.ResponseSort as ResponseSort
import qualified StarTrek.Types.SeasonBase as SeasonBase

data SeasonBaseResponse = SeasonBaseResponse
  { page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  , seasons :: Maybe [SeasonBase.SeasonBase] -- ^ Base season, returned in search results
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  }
  deriving (Eq, Show)

seasonBaseResponseSchema :: FC.Fleece t => FC.Schema t SeasonBaseResponse
seasonBaseResponseSchema =
  FC.object $
    FC.constructor SeasonBaseResponse
      #+ FC.optional "page" page ResponsePage.responsePageSchema
      #+ FC.optional "seasons" seasons (FC.list SeasonBase.seasonBaseSchema)
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema