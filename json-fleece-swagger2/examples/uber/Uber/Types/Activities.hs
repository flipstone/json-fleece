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
  { limit :: Maybe Limit.Limit -- ^ Number of items to retrieve (100 max).
  , count :: Maybe Count.Count -- ^ Total number of items available.
  , history :: Maybe [Activity.Activity]
  , offset :: Maybe Offset.Offset -- ^ Position in pagination.
  }
  deriving (Eq, Show)

activitiesSchema :: FC.Fleece schema => schema Activities
activitiesSchema =
  FC.object $
    FC.constructor Activities
      #+ FC.optional "limit" limit Limit.limitSchema
      #+ FC.optional "count" count Count.countSchema
      #+ FC.optional "history" history (FC.list Activity.activitySchema)
      #+ FC.optional "offset" offset Offset.offsetSchema