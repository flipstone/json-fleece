{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SeasonBaseResponse
  ( SeasonBaseResponse(..)
  , seasonBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)
import StarTrek.SeasonBase (SeasonBase, seasonBaseSchema)

data SeasonBaseResponse = SeasonBaseResponse
  { seasons :: Maybe [SeasonBase] -- ^ List of seasons matching given criteria
  , sort :: Maybe ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

seasonBaseResponseSchema :: FC.Fleece schema => schema SeasonBaseResponse
seasonBaseResponseSchema =
  FC.object $
    FC.constructor SeasonBaseResponse
      #+ FC.optional "seasons" seasons (FC.list seasonBaseSchema)
      #+ FC.optional "sort" sort responseSortSchema
      #+ FC.optional "page" page responsePageSchema