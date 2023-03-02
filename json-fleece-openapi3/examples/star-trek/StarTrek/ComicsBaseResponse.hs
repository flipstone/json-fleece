{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicsBaseResponse
  ( ComicsBaseResponse(..)
  , comicsBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.ComicsBase as ComicsBase
import qualified StarTrek.ResponsePage as ResponsePage
import qualified StarTrek.ResponseSort as ResponseSort

data ComicsBaseResponse = ComicsBaseResponse
  { sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  , comics :: Maybe [ComicsBase.ComicsBase] -- ^ Base comics, returned in search results
  }
  deriving (Eq, Show)

comicsBaseResponseSchema :: FC.Fleece schema => schema ComicsBaseResponse
comicsBaseResponseSchema =
  FC.object $
    FC.constructor ComicsBaseResponse
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema
      #+ FC.optional "page" page ResponsePage.responsePageSchema
      #+ FC.optional "comics" comics (FC.list ComicsBase.comicsBaseSchema)