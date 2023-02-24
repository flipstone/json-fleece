{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ElementBaseResponse
  ( ElementBaseResponse(..)
  , elementBaseResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ElementBase (ElementBase, elementBaseSchema)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)

data ElementBaseResponse = ElementBaseResponse
  { sort :: Maybe ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage -- ^ Object describing response page
  , elements :: Maybe [ElementBase] -- ^ List of elements matching given criteria
  }
  deriving (Eq, Show)

elementBaseResponseSchema :: FC.Fleece schema => schema ElementBaseResponse
elementBaseResponseSchema =
  FC.object $
    FC.constructor ElementBaseResponse
      #+ FC.optionalField FC.OmitKey_DelegateNull "sort" sort responseSortSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "page" page responsePageSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "elements" elements (FC.list elementBaseSchema)