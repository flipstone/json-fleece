{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoGameFull
  ( VideoGameFull(..)
  , videoGameFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.CompanyBase (CompanyBase, companyBaseSchema)
import StarTrek.ContentRating (ContentRating, contentRatingSchema)
import StarTrek.Genre (Genre, genreSchema)
import StarTrek.Platform (Platform, platformSchema)
import StarTrek.Reference (Reference, referenceSchema)
import StarTrek.VideoGameFull.ReleaseDate (ReleaseDate, releaseDateSchema)
import StarTrek.VideoGameFull.StardateFrom (StardateFrom, stardateFromSchema)
import StarTrek.VideoGameFull.StardateTo (StardateTo, stardateToSchema)
import StarTrek.VideoGameFull.SystemRequirements (SystemRequirements, systemRequirementsSchema)
import StarTrek.VideoGameFull.Title (Title, titleSchema)
import StarTrek.VideoGameFull.Uid (Uid, uidSchema)
import StarTrek.VideoGameFull.YearFrom (YearFrom, yearFromSchema)
import StarTrek.VideoGameFull.YearTo (YearTo, yearToSchema)

data VideoGameFull = VideoGameFull
  { yearFrom :: Maybe YearFrom -- ^ Starting year of video game story
  , stardateTo :: Maybe StardateTo -- ^ Ending stardate of video game story
  , publishers :: Maybe [CompanyBase] -- ^ Base company, returned in search results
  , systemRequirements :: Maybe SystemRequirements -- ^ System requirements
  , releaseDate :: Maybe ReleaseDate -- ^ Release date
  , uid :: Uid -- ^ Video game unique ID
  , ratings :: Maybe [ContentRating] -- ^ Rating of video release, etc.
  , stardateFrom :: Maybe StardateFrom -- ^ Starting stardate of video game story
  , genres :: Maybe [Genre] -- ^ Genre of video games
  , title :: Title -- ^ Video game title
  , references :: Maybe [Reference] -- ^ Reference of book, comics, video release, etc.
  , yearTo :: Maybe YearTo -- ^ Ending year of video game story
  , developers :: Maybe [CompanyBase] -- ^ Base company, returned in search results
  , platforms :: Maybe [Platform] -- ^ Platform of video games
  }
  deriving (Eq, Show)

videoGameFullSchema :: FC.Fleece schema => schema VideoGameFull
videoGameFullSchema =
  FC.object $
    FC.constructor VideoGameFull
      #+ FC.optional "yearFrom" yearFrom yearFromSchema
      #+ FC.optional "stardateTo" stardateTo stardateToSchema
      #+ FC.optional "publishers" publishers (FC.list companyBaseSchema)
      #+ FC.optional "systemRequirements" systemRequirements systemRequirementsSchema
      #+ FC.optional "releaseDate" releaseDate releaseDateSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "ratings" ratings (FC.list contentRatingSchema)
      #+ FC.optional "stardateFrom" stardateFrom stardateFromSchema
      #+ FC.optional "genres" genres (FC.list genreSchema)
      #+ FC.required "title" title titleSchema
      #+ FC.optional "references" references (FC.list referenceSchema)
      #+ FC.optional "yearTo" yearTo yearToSchema
      #+ FC.optional "developers" developers (FC.list companyBaseSchema)
      #+ FC.optional "platforms" platforms (FC.list platformSchema)