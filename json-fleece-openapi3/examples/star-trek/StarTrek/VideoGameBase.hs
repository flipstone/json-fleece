{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoGameBase
  ( VideoGameBase(..)
  , videoGameBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.VideoGameBase.ReleaseDate (ReleaseDate, releaseDateSchema)
import StarTrek.VideoGameBase.StardateFrom (StardateFrom, stardateFromSchema)
import StarTrek.VideoGameBase.StardateTo (StardateTo, stardateToSchema)
import StarTrek.VideoGameBase.SystemRequirements (SystemRequirements, systemRequirementsSchema)
import StarTrek.VideoGameBase.Title (Title, titleSchema)
import StarTrek.VideoGameBase.Uid (Uid, uidSchema)
import StarTrek.VideoGameBase.YearFrom (YearFrom, yearFromSchema)
import StarTrek.VideoGameBase.YearTo (YearTo, yearToSchema)

data VideoGameBase = VideoGameBase
  { yearFrom :: Maybe YearFrom -- ^ Starting year of video game story
  , stardateTo :: Maybe StardateTo -- ^ Ending stardate of video game story
  , systemRequirements :: Maybe SystemRequirements -- ^ System requirements
  , releaseDate :: Maybe ReleaseDate -- ^ Release date
  , uid :: Uid -- ^ Video game unique ID
  , stardateFrom :: Maybe StardateFrom -- ^ Starting stardate of video game story
  , title :: Title -- ^ Video game title
  , yearTo :: Maybe YearTo -- ^ Ending year of video game story
  }
  deriving (Eq, Show)

videoGameBaseSchema :: FC.Fleece schema => schema VideoGameBase
videoGameBaseSchema =
  FC.object $
    FC.constructor VideoGameBase
      #+ FC.optional "yearFrom" yearFrom yearFromSchema
      #+ FC.optional "stardateTo" stardateTo stardateToSchema
      #+ FC.optional "systemRequirements" systemRequirements systemRequirementsSchema
      #+ FC.optional "releaseDate" releaseDate releaseDateSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "stardateFrom" stardateFrom stardateFromSchema
      #+ FC.required "title" title titleSchema
      #+ FC.optional "yearTo" yearTo yearToSchema