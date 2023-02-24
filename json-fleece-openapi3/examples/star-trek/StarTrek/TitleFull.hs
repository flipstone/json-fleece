{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TitleFull
  ( TitleFull(..)
  , titleFullSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
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
      #+ FC.optionalField FC.OmitKey_DelegateNull "militaryRank" militaryRank FC.boolean
      #+ FC.required "name" name FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "religiousTitle" religiousTitle FC.boolean
      #+ FC.required "uid" uid FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "mirror" mirror FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "characters" characters (FC.list characterBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "fleetRank" fleetRank FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "position" position FC.boolean