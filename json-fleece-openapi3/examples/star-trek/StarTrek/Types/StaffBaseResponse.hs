{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffBaseResponse
  ( StaffBaseResponse(..)
  , staffBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.ResponsePage as ResponsePage
import qualified StarTrek.Types.ResponseSort as ResponseSort
import qualified StarTrek.Types.StaffBase as StaffBase

data StaffBaseResponse = StaffBaseResponse
  { sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  , staff :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  }
  deriving (Eq, Show)

staffBaseResponseSchema :: FC.Fleece schema => schema StaffBaseResponse
staffBaseResponseSchema =
  FC.object $
    FC.constructor StaffBaseResponse
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema
      #+ FC.optional "page" page ResponsePage.responsePageSchema
      #+ FC.optional "staff" staff (FC.list StaffBase.staffBaseSchema)