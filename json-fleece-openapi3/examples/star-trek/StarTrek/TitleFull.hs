{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TitleFull
  ( TitleFull(..)
  , titleFullSchema
  ) where

import Data.Text (Text)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Bool, Eq, Maybe, Show)
import StarTrek.CharacterBase (CharacterBase, characterBaseSchema)

data TitleFull = TitleFull
  { militaryRank :: Maybe Bool -- ^ Whether it's a military rank
  , name :: Text -- ^ Title name
  , religiousTitle :: Maybe Bool -- ^ Whether it's a religious title
  , uid :: Text -- ^ Title unique ID
  , mirror :: Maybe Bool -- ^ Whether this title is from mirror universe
  , characters :: Maybe [CharacterBase] -- ^ Characters that holds this title
  , fleetRank :: Maybe Bool -- ^ Whether it's a fleet rank
  , position :: Maybe Bool -- ^ Whether it's a position
  }
  deriving (Eq, Show)

titleFullSchema :: FC.Fleece schema => schema TitleFull
titleFullSchema =
  FC.object $
    FC.constructor TitleFull
      #+ FC.optional "militaryRank" militaryRank FC.boolean
      #+ FC.required "name" name FC.text
      #+ FC.optional "religiousTitle" religiousTitle FC.boolean
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "mirror" mirror FC.boolean
      #+ FC.optional "characters" characters (FC.list characterBaseSchema)
      #+ FC.optional "fleetRank" fleetRank FC.boolean
      #+ FC.optional "position" position FC.boolean