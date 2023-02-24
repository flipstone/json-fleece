{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.LiteratureBaseResponse
  ( LiteratureBaseResponse(..)
  , literatureBaseResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.LiteratureBase (LiteratureBase, literatureBaseSchema)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)

data LiteratureBaseResponse = LiteratureBaseResponse
  { sort :: Maybe ResponseSort -- ^ Response sort
  , literature :: Maybe [LiteratureBase] -- ^ List of literature matching given criteria
  , page :: Maybe ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

literatureBaseResponseSchema :: FC.Fleece schema => schema LiteratureBaseResponse
literatureBaseResponseSchema =
  FC.object $
    FC.constructor LiteratureBaseResponse
      #+ FC.optionalField FC.OmitKey_DelegateNull "sort" sort responseSortSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "literature" literature (FC.list literatureBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "page" page responsePageSchema