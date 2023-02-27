{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TitleBase
  ( TitleBase(..)
  , titleBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.TitleBase.FleetRank (FleetRank, fleetRankSchema)
import StarTrek.TitleBase.MilitaryRank (MilitaryRank, militaryRankSchema)
import StarTrek.TitleBase.Mirror (Mirror, mirrorSchema)
import StarTrek.TitleBase.Name (Name, nameSchema)
import StarTrek.TitleBase.Position (Position, positionSchema)
import StarTrek.TitleBase.ReligiousTitle (ReligiousTitle, religiousTitleSchema)
import StarTrek.TitleBase.Uid (Uid, uidSchema)

data TitleBase = TitleBase
  { militaryRank :: Maybe MilitaryRank -- ^ Whether it's a military rank
  , name :: Name -- ^ Title name
  , religiousTitle :: Maybe ReligiousTitle -- ^ Whether it's a religious title
  , uid :: Uid -- ^ Title unique ID
  , mirror :: Maybe Mirror -- ^ Whether this title is from mirror universe
  , fleetRank :: Maybe FleetRank -- ^ Whether it's a fleet rank
  , position :: Maybe Position -- ^ Whether it's a position
  }
  deriving (Eq, Show)

titleBaseSchema :: FC.Fleece schema => schema TitleBase
titleBaseSchema =
  FC.object $
    FC.constructor TitleBase
      #+ FC.optional "militaryRank" militaryRank militaryRankSchema
      #+ FC.required "name" name nameSchema
      #+ FC.optional "religiousTitle" religiousTitle religiousTitleSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "mirror" mirror mirrorSchema
      #+ FC.optional "fleetRank" fleetRank fleetRankSchema
      #+ FC.optional "position" position positionSchema