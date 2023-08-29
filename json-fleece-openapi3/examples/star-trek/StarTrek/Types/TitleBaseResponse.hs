{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TitleBaseResponse
  ( TitleBaseResponse(..)
  , titleBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.ResponsePage as ResponsePage
import qualified StarTrek.Types.ResponseSort as ResponseSort
import qualified StarTrek.Types.TitleBase as TitleBase

data TitleBaseResponse = TitleBaseResponse
  { page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  , titles :: Maybe [TitleBase.TitleBase] -- ^ Base title, returned in search results
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  }
  deriving (Eq, Show)

titleBaseResponseSchema :: FC.Fleece schema => schema TitleBaseResponse
titleBaseResponseSchema =
  FC.object $
    FC.constructor TitleBaseResponse
      #+ FC.optional "page" page ResponsePage.responsePageSchema
      #+ FC.optional "titles" titles (FC.list TitleBase.titleBaseSchema)
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema