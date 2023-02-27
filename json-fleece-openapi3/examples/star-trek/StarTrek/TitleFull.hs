{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TitleFull
  ( TitleFull(..)
  , titleFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.CharacterBase (CharacterBase, characterBaseSchema)
import StarTrek.TitleFull.FleetRank (FleetRank, fleetRankSchema)
import StarTrek.TitleFull.MilitaryRank (MilitaryRank, militaryRankSchema)
import StarTrek.TitleFull.Mirror (Mirror, mirrorSchema)
import StarTrek.TitleFull.Name (Name, nameSchema)
import StarTrek.TitleFull.Position (Position, positionSchema)
import StarTrek.TitleFull.ReligiousTitle (ReligiousTitle, religiousTitleSchema)
import StarTrek.TitleFull.Uid (Uid, uidSchema)

data TitleFull = TitleFull
  { militaryRank :: Maybe MilitaryRank -- ^ Whether it's a military rank
  , name :: Name -- ^ Title name
  , religiousTitle :: Maybe ReligiousTitle -- ^ Whether it's a religious title
  , uid :: Uid -- ^ Title unique ID
  , mirror :: Maybe Mirror -- ^ Whether this title is from mirror universe
  , characters :: Maybe [CharacterBase] -- ^ Base character, returned in search results
  , fleetRank :: Maybe FleetRank -- ^ Whether it's a fleet rank
  , position :: Maybe Position -- ^ Whether it's a position
  }
  deriving (Eq, Show)

titleFullSchema :: FC.Fleece schema => schema TitleFull
titleFullSchema =
  FC.object $
    FC.constructor TitleFull
      #+ FC.optional "militaryRank" militaryRank militaryRankSchema
      #+ FC.required "name" name nameSchema
      #+ FC.optional "religiousTitle" religiousTitle religiousTitleSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "mirror" mirror mirrorSchema
      #+ FC.optional "characters" characters (FC.list characterBaseSchema)
      #+ FC.optional "fleetRank" fleetRank fleetRankSchema
      #+ FC.optional "position" position positionSchema