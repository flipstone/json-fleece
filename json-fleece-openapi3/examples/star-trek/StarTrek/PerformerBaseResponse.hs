{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.PerformerBaseResponse
  ( PerformerBaseResponse(..)
  , performerBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.PerformerBase (PerformerBase, performerBaseSchema)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)

data PerformerBaseResponse = PerformerBaseResponse
  { performers :: Maybe [PerformerBase] -- ^ Base performer, returned in search results
  , sort :: Maybe ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

performerBaseResponseSchema :: FC.Fleece schema => schema PerformerBaseResponse
performerBaseResponseSchema =
  FC.object $
    FC.constructor PerformerBaseResponse
      #+ FC.optional "performers" performers (FC.list performerBaseSchema)
      #+ FC.optional "sort" sort responseSortSchema
      #+ FC.optional "page" page responsePageSchema