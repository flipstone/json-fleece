{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TitleBase
  ( TitleBase(..)
  , titleBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.TitleBase.FleetRank as FleetRank
import qualified StarTrek.Types.TitleBase.MilitaryRank as MilitaryRank
import qualified StarTrek.Types.TitleBase.Mirror as Mirror
import qualified StarTrek.Types.TitleBase.Name as Name
import qualified StarTrek.Types.TitleBase.Position as Position
import qualified StarTrek.Types.TitleBase.ReligiousTitle as ReligiousTitle
import qualified StarTrek.Types.TitleBase.Uid as Uid

data TitleBase = TitleBase
  { mirror :: Maybe Mirror.Mirror -- ^ Whether this title is from mirror universe
  , uid :: Uid.Uid -- ^ Title unique ID
  , fleetRank :: Maybe FleetRank.FleetRank -- ^ Whether it's a fleet rank
  , militaryRank :: Maybe MilitaryRank.MilitaryRank -- ^ Whether it's a military rank
  , religiousTitle :: Maybe ReligiousTitle.ReligiousTitle -- ^ Whether it's a religious title
  , name :: Name.Name -- ^ Title name
  , position :: Maybe Position.Position -- ^ Whether it's a position
  }
  deriving (Eq, Show)

titleBaseSchema :: FC.Fleece schema => schema TitleBase
titleBaseSchema =
  FC.object $
    FC.constructor TitleBase
      #+ FC.optional "mirror" mirror Mirror.mirrorSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "fleetRank" fleetRank FleetRank.fleetRankSchema
      #+ FC.optional "militaryRank" militaryRank MilitaryRank.militaryRankSchema
      #+ FC.optional "religiousTitle" religiousTitle ReligiousTitle.religiousTitleSchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "position" position Position.positionSchema