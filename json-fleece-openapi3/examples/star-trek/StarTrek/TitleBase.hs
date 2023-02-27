{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TitleBase
  ( TitleBase(..)
  , titleBaseSchema
  ) where

import Data.Text (Text)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Bool, Eq, Maybe, Show)

data TitleBase = TitleBase
  { militaryRank :: Maybe Bool -- ^ Whether it's a military rank
  , name :: Text -- ^ Title name
  , religiousTitle :: Maybe Bool -- ^ Whether it's a religious title
  , uid :: Text -- ^ Title unique ID
  , mirror :: Maybe Bool -- ^ Whether this title is from mirror universe
  , fleetRank :: Maybe Bool -- ^ Whether it's a fleet rank
  , position :: Maybe Bool -- ^ Whether it's a position
  }
  deriving (Eq, Show)

titleBaseSchema :: FC.Fleece schema => schema TitleBase
titleBaseSchema =
  FC.object $
    FC.constructor TitleBase
      #+ FC.optional "militaryRank" militaryRank FC.boolean
      #+ FC.required "name" name FC.text
      #+ FC.optional "religiousTitle" religiousTitle FC.boolean
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "mirror" mirror FC.boolean
      #+ FC.optional "fleetRank" fleetRank FC.boolean
      #+ FC.optional "position" position FC.boolean