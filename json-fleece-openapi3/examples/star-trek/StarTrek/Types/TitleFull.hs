{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TitleFull
  ( TitleFull(..)
  , titleFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.CharacterBase as CharacterBase
import qualified StarTrek.Types.TitleFull.FleetRank as FleetRank
import qualified StarTrek.Types.TitleFull.MilitaryRank as MilitaryRank
import qualified StarTrek.Types.TitleFull.Mirror as Mirror
import qualified StarTrek.Types.TitleFull.Name as Name
import qualified StarTrek.Types.TitleFull.Position as Position
import qualified StarTrek.Types.TitleFull.ReligiousTitle as ReligiousTitle
import qualified StarTrek.Types.TitleFull.Uid as Uid

data TitleFull = TitleFull
  { characters :: Maybe [CharacterBase.CharacterBase] -- ^ Base character, returned in search results
  , fleetRank :: Maybe FleetRank.FleetRank -- ^ Whether it's a fleet rank
  , militaryRank :: Maybe MilitaryRank.MilitaryRank -- ^ Whether it's a military rank
  , mirror :: Maybe Mirror.Mirror -- ^ Whether this title is from mirror universe
  , name :: Name.Name -- ^ Title name
  , position :: Maybe Position.Position -- ^ Whether it's a position
  , religiousTitle :: Maybe ReligiousTitle.ReligiousTitle -- ^ Whether it's a religious title
  , uid :: Uid.Uid -- ^ Title unique ID
  }
  deriving (Eq, Show)

titleFullSchema :: FC.Fleece t => FC.Schema t TitleFull
titleFullSchema =
  FC.object $
    FC.constructor TitleFull
      #+ FC.optional "characters" characters (FC.list CharacterBase.characterBaseSchema)
      #+ FC.optional "fleetRank" fleetRank FleetRank.fleetRankSchema
      #+ FC.optional "militaryRank" militaryRank MilitaryRank.militaryRankSchema
      #+ FC.optional "mirror" mirror Mirror.mirrorSchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "position" position Position.positionSchema
      #+ FC.optional "religiousTitle" religiousTitle ReligiousTitle.religiousTitleSchema
      #+ FC.required "uid" uid Uid.uidSchema