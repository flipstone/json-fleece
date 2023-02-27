{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffBaseResponse
  ( StaffBaseResponse(..)
  , staffBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)
import StarTrek.StaffBase (StaffBase, staffBaseSchema)

data StaffBaseResponse = StaffBaseResponse
  { sort :: Maybe ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage -- ^ Object describing response page
  , staff :: Maybe [StaffBase] -- ^ Base staff, returned in search results
  }
  deriving (Eq, Show)

staffBaseResponseSchema :: FC.Fleece schema => schema StaffBaseResponse
staffBaseResponseSchema =
  FC.object $
    FC.constructor StaffBaseResponse
      #+ FC.optional "sort" sort responseSortSchema
      #+ FC.optional "page" page responsePageSchema
      #+ FC.optional "staff" staff (FC.list staffBaseSchema)