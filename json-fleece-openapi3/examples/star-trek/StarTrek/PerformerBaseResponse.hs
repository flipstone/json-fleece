{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.PerformerBaseResponse
  ( PerformerBaseResponse(..)
  , performerBaseResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.PerformerBase (PerformerBase, performerBaseSchema)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)

data PerformerBaseResponse = PerformerBaseResponse
  { performers :: Maybe [PerformerBase] -- ^ List of performers matching given criteria
  , sort :: Maybe ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

performerBaseResponseSchema :: FC.Fleece schema => schema PerformerBaseResponse
performerBaseResponseSchema =
  FC.object $
    FC.constructor PerformerBaseResponse
      #+ FC.optionalField FC.OmitKey_DelegateNull "performers" performers (FC.list performerBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "sort" sort responseSortSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "page" page responsePageSchema