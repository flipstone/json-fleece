{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ComicsBaseResponse
  ( ComicsBaseResponse(..)
  , comicsBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.ComicsBase as ComicsBase
import qualified StarTrek.Types.ResponsePage as ResponsePage
import qualified StarTrek.Types.ResponseSort as ResponseSort

data ComicsBaseResponse = ComicsBaseResponse
  { comics :: Maybe [ComicsBase.ComicsBase] -- ^ Base comics, returned in search results
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  }
  deriving (Eq, Show)

comicsBaseResponseSchema :: FC.Fleece schema => schema ComicsBaseResponse
comicsBaseResponseSchema =
  FC.object $
    FC.constructor ComicsBaseResponse
      #+ FC.optional "comics" comics (FC.list ComicsBase.comicsBaseSchema)
      #+ FC.optional "page" page ResponsePage.responsePageSchema
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema