{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.PerformerBaseResponse
  ( PerformerBaseResponse(..)
  , performerBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.PerformerBase as PerformerBase
import qualified StarTrek.ResponsePage as ResponsePage
import qualified StarTrek.ResponseSort as ResponseSort

data PerformerBaseResponse = PerformerBaseResponse
  { performers :: Maybe [PerformerBase.PerformerBase] -- ^ Base performer, returned in search results
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

performerBaseResponseSchema :: FC.Fleece schema => schema PerformerBaseResponse
performerBaseResponseSchema =
  FC.object $
    FC.constructor PerformerBaseResponse
      #+ FC.optional "performers" performers (FC.list PerformerBase.performerBaseSchema)
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema
      #+ FC.optional "page" page ResponsePage.responsePageSchema