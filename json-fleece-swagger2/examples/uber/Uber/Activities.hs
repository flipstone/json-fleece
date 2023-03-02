{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Activities
  ( Activities(..)
  , activitiesSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified Uber.Activities.Count as Count
import qualified Uber.Activities.Limit as Limit
import qualified Uber.Activities.Offset as Offset
import qualified Uber.Activity as Activity

data Activities = Activities
  { offset :: Maybe Offset.Offset -- ^ Position in pagination.
  , count :: Maybe Count.Count -- ^ Total number of items available.
  , limit :: Maybe Limit.Limit -- ^ Number of items to retrieve (100 max).
  , history :: Maybe [Activity.Activity]
  }
  deriving (Eq, Show)

activitiesSchema :: FC.Fleece schema => schema Activities
activitiesSchema =
  FC.object $
    FC.constructor Activities
      #+ FC.optional "offset" offset Offset.offsetSchema
      #+ FC.optional "count" count Count.countSchema
      #+ FC.optional "limit" limit Limit.limitSchema
      #+ FC.optional "history" history (FC.list Activity.activitySchema)