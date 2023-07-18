{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Types.Activities
  ( Activities(..)
  , activitiesSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified Uber.Types.Activities.Count as Count
import qualified Uber.Types.Activities.Limit as Limit
import qualified Uber.Types.Activities.Offset as Offset
import qualified Uber.Types.Activity as Activity

data Activities = Activities
  { offset :: Maybe Offset.Offset -- ^ Position in pagination.
  , limit :: Maybe Limit.Limit -- ^ Number of items to retrieve (100 max).
  , history :: Maybe [Activity.Activity]
  , count :: Maybe Count.Count -- ^ Total number of items available.
  }
  deriving (Eq, Show)

activitiesSchema :: FC.Fleece schema => schema Activities
activitiesSchema =
  FC.object $
    FC.constructor Activities
      #+ FC.optional "offset" offset Offset.offsetSchema
      #+ FC.optional "limit" limit Limit.limitSchema
      #+ FC.optional "history" history (FC.list Activity.activitySchema)
      #+ FC.optional "count" count Count.countSchema