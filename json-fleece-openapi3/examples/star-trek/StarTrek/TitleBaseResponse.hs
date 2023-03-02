{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TitleBaseResponse
  ( TitleBaseResponse(..)
  , titleBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.ResponsePage as ResponsePage
import qualified StarTrek.ResponseSort as ResponseSort
import qualified StarTrek.TitleBase as TitleBase

data TitleBaseResponse = TitleBaseResponse
  { sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  , titles :: Maybe [TitleBase.TitleBase] -- ^ Base title, returned in search results
  }
  deriving (Eq, Show)

titleBaseResponseSchema :: FC.Fleece schema => schema TitleBaseResponse
titleBaseResponseSchema =
  FC.object $
    FC.constructor TitleBaseResponse
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema
      #+ FC.optional "page" page ResponsePage.responsePageSchema
      #+ FC.optional "titles" titles (FC.list TitleBase.titleBaseSchema)