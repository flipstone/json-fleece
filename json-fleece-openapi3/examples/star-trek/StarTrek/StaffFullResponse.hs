{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffFullResponse
  ( StaffFullResponse(..)
  , staffFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.StaffFull as StaffFull

data StaffFullResponse = StaffFullResponse
  { staff :: Maybe StaffFull.StaffFull -- ^ Full staff, returned when queried using UID
  }
  deriving (Eq, Show)

staffFullResponseSchema :: FC.Fleece schema => schema StaffFullResponse
staffFullResponseSchema =
  FC.object $
    FC.constructor StaffFullResponse
      #+ FC.optional "staff" staff StaffFull.staffFullSchema