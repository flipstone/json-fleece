{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffBaseResponse
  ( StaffBaseResponse(..)
  , staffBaseResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)
import StarTrek.StaffBase (StaffBase, staffBaseSchema)

data StaffBaseResponse = StaffBaseResponse
  { sort :: Maybe ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage -- ^ Object describing response page
  , staff :: Maybe [StaffBase] -- ^ List of staff matching given criteria
  }
  deriving (Eq, Show)

staffBaseResponseSchema :: FC.Fleece schema => schema StaffBaseResponse
staffBaseResponseSchema =
  FC.object $
    FC.constructor StaffBaseResponse
      #+ FC.optionalField FC.OmitKey_DelegateNull "sort" sort responseSortSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "page" page responsePageSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "staff" staff (FC.list staffBaseSchema)