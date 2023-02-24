{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SeasonBaseResponse
  ( SeasonBaseResponse(..)
  , seasonBaseResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
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
      #+ FC.optionalField FC.OmitKey_DelegateNull "seasons" seasons (FC.list seasonBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "sort" sort responseSortSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "page" page responsePageSchema