{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SeasonBaseResponse
  ( SeasonBaseResponse(..)
  , seasonBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.ResponsePage as ResponsePage
import qualified StarTrek.ResponseSort as ResponseSort
import qualified StarTrek.SeasonBase as SeasonBase

data SeasonBaseResponse = SeasonBaseResponse
  { seasons :: Maybe [SeasonBase.SeasonBase] -- ^ Base season, returned in search results
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

seasonBaseResponseSchema :: FC.Fleece schema => schema SeasonBaseResponse
seasonBaseResponseSchema =
  FC.object $
    FC.constructor SeasonBaseResponse
      #+ FC.optional "seasons" seasons (FC.list SeasonBase.seasonBaseSchema)
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema
      #+ FC.optional "page" page ResponsePage.responsePageSchema