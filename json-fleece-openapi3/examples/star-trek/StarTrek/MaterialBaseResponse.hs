{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MaterialBaseResponse
  ( MaterialBaseResponse(..)
  , materialBaseResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.MaterialBase (MaterialBase, materialBaseSchema)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)

data MaterialBaseResponse = MaterialBaseResponse
  { sort :: Maybe ResponseSort -- ^ Response sort
  , materials :: Maybe [MaterialBase] -- ^ List of materials matching given criteria
  , page :: Maybe ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

materialBaseResponseSchema :: FC.Fleece schema => schema MaterialBaseResponse
materialBaseResponseSchema =
  FC.object $
    FC.constructor MaterialBaseResponse
      #+ FC.optionalField FC.OmitKey_DelegateNull "sort" sort responseSortSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "materials" materials (FC.list materialBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "page" page responsePageSchema