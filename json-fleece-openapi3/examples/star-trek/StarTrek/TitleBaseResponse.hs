{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TitleBaseResponse
  ( TitleBaseResponse(..)
  , titleBaseResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)
import StarTrek.TitleBase (TitleBase, titleBaseSchema)

data TitleBaseResponse = TitleBaseResponse
  { sort :: Maybe ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage -- ^ Object describing response page
  , titles :: Maybe [TitleBase] -- ^ List of titles matching given criteria
  }
  deriving (Eq, Show)

titleBaseResponseSchema :: FC.Fleece schema => schema TitleBaseResponse
titleBaseResponseSchema =
  FC.object $
    FC.constructor TitleBaseResponse
      #+ FC.optionalField FC.OmitKey_DelegateNull "sort" sort responseSortSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "page" page responsePageSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "titles" titles (FC.list titleBaseSchema)