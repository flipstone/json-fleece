{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TitleBaseResponse
  ( TitleBaseResponse(..)
  , titleBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)
import StarTrek.TitleBase (TitleBase, titleBaseSchema)

data TitleBaseResponse = TitleBaseResponse
  { sort :: Maybe ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage -- ^ Object describing response page
  , titles :: Maybe [TitleBase] -- ^ Base title, returned in search results
  }
  deriving (Eq, Show)

titleBaseResponseSchema :: FC.Fleece schema => schema TitleBaseResponse
titleBaseResponseSchema =
  FC.object $
    FC.constructor TitleBaseResponse
      #+ FC.optional "sort" sort responseSortSchema
      #+ FC.optional "page" page responsePageSchema
      #+ FC.optional "titles" titles (FC.list titleBaseSchema)