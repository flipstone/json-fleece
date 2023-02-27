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
      #+ FC.optional "sort" sort responseSortSchema
      #+ FC.optional "page" page responsePageSchema
      #+ FC.optional "comics" comics (FC.list comicsBaseSchema)