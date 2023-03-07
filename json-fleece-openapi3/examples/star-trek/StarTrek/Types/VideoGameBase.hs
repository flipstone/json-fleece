{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.VideoGameBase
  ( VideoGameBase(..)
  , videoGameBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.VideoGameBase.ReleaseDate as ReleaseDate
import qualified StarTrek.Types.VideoGameBase.StardateFrom as StardateFrom
import qualified StarTrek.Types.VideoGameBase.StardateTo as StardateTo
import qualified StarTrek.Types.VideoGameBase.SystemRequirements as SystemRequirements
import qualified StarTrek.Types.VideoGameBase.Title as Title
import qualified StarTrek.Types.VideoGameBase.Uid as Uid
import qualified StarTrek.Types.VideoGameBase.YearFrom as YearFrom
import qualified StarTrek.Types.VideoGameBase.YearTo as YearTo

data VideoGameBase = VideoGameBase
  { yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of video game story
  , stardateTo :: Maybe StardateTo.StardateTo -- ^ Ending stardate of video game story
  , systemRequirements :: Maybe SystemRequirements.SystemRequirements -- ^ System requirements
  , releaseDate :: Maybe ReleaseDate.ReleaseDate -- ^ Release date
  , uid :: Uid.Uid -- ^ Video game unique ID
  , stardateFrom :: Maybe StardateFrom.StardateFrom -- ^ Starting stardate of video game story
  , title :: Title.Title -- ^ Video game title
  , yearTo :: Maybe YearTo.YearTo -- ^ Ending year of video game story
  }
  deriving (Eq, Show)

videoGameBaseSchema :: FC.Fleece schema => schema VideoGameBase
videoGameBaseSchema =
  FC.object $
    FC.constructor VideoGameBase
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.optional "stardateTo" stardateTo StardateTo.stardateToSchema
      #+ FC.optional "systemRequirements" systemRequirements SystemRequirements.systemRequirementsSchema
      #+ FC.optional "releaseDate" releaseDate ReleaseDate.releaseDateSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "stardateFrom" stardateFrom StardateFrom.stardateFromSchema
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema