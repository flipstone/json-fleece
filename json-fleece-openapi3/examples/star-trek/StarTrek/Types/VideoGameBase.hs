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
  { title :: Title.Title -- ^ Video game title
  , yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of video game story
  , systemRequirements :: Maybe SystemRequirements.SystemRequirements -- ^ System requirements
  , uid :: Uid.Uid -- ^ Video game unique ID
  , stardateFrom :: Maybe StardateFrom.StardateFrom -- ^ Starting stardate of video game story
  , releaseDate :: Maybe ReleaseDate.ReleaseDate -- ^ Release date
  , yearTo :: Maybe YearTo.YearTo -- ^ Ending year of video game story
  , stardateTo :: Maybe StardateTo.StardateTo -- ^ Ending stardate of video game story
  }
  deriving (Eq, Show)

videoGameBaseSchema :: FC.Fleece schema => schema VideoGameBase
videoGameBaseSchema =
  FC.object $
    FC.constructor VideoGameBase
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.optional "systemRequirements" systemRequirements SystemRequirements.systemRequirementsSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "stardateFrom" stardateFrom StardateFrom.stardateFromSchema
      #+ FC.optional "releaseDate" releaseDate ReleaseDate.releaseDateSchema
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema
      #+ FC.optional "stardateTo" stardateTo StardateTo.stardateToSchema