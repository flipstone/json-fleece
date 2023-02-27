{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Activities
  ( Activities(..)
  , activitiesSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import Uber.Activities.Count (Count, countSchema)
import Uber.Activities.Limit (Limit, limitSchema)
import Uber.Activities.Offset (Offset, offsetSchema)
import Uber.Activity (Activity, activitySchema)

data Activities = Activities
  { offset :: Maybe Offset -- ^ Position in pagination.
  , count :: Maybe Count -- ^ Total number of items available.
  , limit :: Maybe Limit -- ^ Number of items to retrieve (100 max).
  , history :: Maybe [Activity]
  }
  deriving (Eq, Show)

activitiesSchema :: FC.Fleece schema => schema Activities
activitiesSchema =
  FC.object $
    FC.constructor Activities
      #+ FC.optional "offset" offset offsetSchema
      #+ FC.optional "count" count countSchema
      #+ FC.optional "limit" limit limitSchema
      #+ FC.optional "history" history (FC.list activitySchema)