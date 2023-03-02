{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoGameFull
  ( VideoGameFull(..)
  , videoGameFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.CompanyBase as CompanyBase
import qualified StarTrek.ContentRating as ContentRating
import qualified StarTrek.Genre as Genre
import qualified StarTrek.Platform as Platform
import qualified StarTrek.Reference as Reference
import qualified StarTrek.VideoGameFull.ReleaseDate as ReleaseDate
import qualified StarTrek.VideoGameFull.StardateFrom as StardateFrom
import qualified StarTrek.VideoGameFull.StardateTo as StardateTo
import qualified StarTrek.VideoGameFull.SystemRequirements as SystemRequirements
import qualified StarTrek.VideoGameFull.Title as Title
import qualified StarTrek.VideoGameFull.Uid as Uid
import qualified StarTrek.VideoGameFull.YearFrom as YearFrom
import qualified StarTrek.VideoGameFull.YearTo as YearTo

data VideoGameFull = VideoGameFull
  { yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of video game story
  , stardateTo :: Maybe StardateTo.StardateTo -- ^ Ending stardate of video game story
  , publishers :: Maybe [CompanyBase.CompanyBase] -- ^ Base company, returned in search results
  , systemRequirements :: Maybe SystemRequirements.SystemRequirements -- ^ System requirements
  , releaseDate :: Maybe ReleaseDate.ReleaseDate -- ^ Release date
  , uid :: Uid.Uid -- ^ Video game unique ID
  , ratings :: Maybe [ContentRating.ContentRating] -- ^ Rating of video release, etc.
  , stardateFrom :: Maybe StardateFrom.StardateFrom -- ^ Starting stardate of video game story
  , genres :: Maybe [Genre.Genre] -- ^ Genre of video games
  , title :: Title.Title -- ^ Video game title
  , references :: Maybe [Reference.Reference] -- ^ Reference of book, comics, video release, etc.
  , yearTo :: Maybe YearTo.YearTo -- ^ Ending year of video game story
  , developers :: Maybe [CompanyBase.CompanyBase] -- ^ Base company, returned in search results
  , platforms :: Maybe [Platform.Platform] -- ^ Platform of video games
  }
  deriving (Eq, Show)

videoGameFullSchema :: FC.Fleece schema => schema VideoGameFull
videoGameFullSchema =
  FC.object $
    FC.constructor VideoGameFull
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.optional "stardateTo" stardateTo StardateTo.stardateToSchema
      #+ FC.optional "publishers" publishers (FC.list CompanyBase.companyBaseSchema)
      #+ FC.optional "systemRequirements" systemRequirements SystemRequirements.systemRequirementsSchema
      #+ FC.optional "releaseDate" releaseDate ReleaseDate.releaseDateSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "ratings" ratings (FC.list ContentRating.contentRatingSchema)
      #+ FC.optional "stardateFrom" stardateFrom StardateFrom.stardateFromSchema
      #+ FC.optional "genres" genres (FC.list Genre.genreSchema)
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "references" references (FC.list Reference.referenceSchema)
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema
      #+ FC.optional "developers" developers (FC.list CompanyBase.companyBaseSchema)
      #+ FC.optional "platforms" platforms (FC.list Platform.platformSchema)