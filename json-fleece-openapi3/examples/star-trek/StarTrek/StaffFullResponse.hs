{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffFullResponse
  ( StaffFullResponse(..)
  , staffFullResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.StaffFull (StaffFull, staffFullSchema)

data StaffFullResponse = StaffFullResponse
  { staff :: Maybe StaffFull -- ^ Full staff, returned when queried using UID
  }
  deriving (Eq, Show)

staffFullResponseSchema :: FC.Fleece schema => schema StaffFullResponse
staffFullResponseSchema =
  FC.object $
    FC.constructor StaffFullResponse
      #+ FC.optionalField FC.OmitKey_DelegateNull "staff" staff staffFullSchema