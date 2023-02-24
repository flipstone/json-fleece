{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicsBaseResponse
  ( ComicsBaseResponse(..)
  , comicsBaseResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ComicsBase (ComicsBase, comicsBaseSchema)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)

data ComicsBaseResponse = ComicsBaseResponse
  { sort :: Maybe ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage -- ^ Object describing response page
  , comics :: Maybe [ComicsBase] -- ^ List of comics matching given criteria
  }
  deriving (Eq, Show)

comicsBaseResponseSchema :: FC.Fleece schema => schema ComicsBaseResponse
comicsBaseResponseSchema =
  FC.object $
    FC.constructor ComicsBaseResponse
      #+ FC.optionalField FC.OmitKey_DelegateNull "sort" sort responseSortSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "page" page responsePageSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "comics" comics (FC.list comicsBaseSchema)