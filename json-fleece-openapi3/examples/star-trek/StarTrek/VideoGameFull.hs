{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoGameFull
  ( VideoGameFull(..)
  , videoGameFullSchema
  ) where

import qualified Fleece.Core as FC
import Data.Scientific (Scientific)
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Integer, Maybe, Show)
import StarTrek.CompanyBase (CompanyBase, companyBaseSchema)
import StarTrek.ContentRating (ContentRating, contentRatingSchema)
import StarTrek.Genre (Genre, genreSchema)
import StarTrek.Platform (Platform, platformSchema)
import StarTrek.Reference (Reference, referenceSchema)

data VideoGameFull = VideoGameFull
  { yearFrom :: Maybe Integer -- ^ Starting year of video game story
  , stardateTo :: Maybe Scientific -- ^ Ending stardate of video game story
  , publishers :: Maybe [CompanyBase] -- ^ Publishers
  , systemRequirements :: Maybe Text -- ^ System requirements
  , releaseDate :: Maybe Text -- ^ Release date
  , uid :: Text -- ^ Video game unique ID
  , ratings :: Maybe [ContentRating] -- ^ Ratings
  , stardateFrom :: Maybe Scientific -- ^ Starting stardate of video game story
  , genres :: Maybe [Genre] -- ^ Genres
  , title :: Text -- ^ Video game title
  , references :: Maybe [Reference] -- ^ References
  , yearTo :: Maybe Integer -- ^ Ending year of video game story
  , developers :: Maybe [CompanyBase] -- ^ Developers
  , platforms :: Maybe [Platform] -- ^ Platforms
  }
  deriving (Eq, Show)

videoGameFullSchema :: FC.Fleece schema => schema VideoGameFull
videoGameFullSchema =
  FC.object $
    FC.constructor VideoGameFull
      #+ FC.optionalField FC.OmitKey_DelegateNull "yearFrom" yearFrom FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "stardateTo" stardateTo FC.number
      #+ FC.optionalField FC.OmitKey_DelegateNull "publishers" publishers (FC.list companyBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "systemRequirements" systemRequirements FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "releaseDate" releaseDate FC.text
      #+ FC.required "uid" uid FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "ratings" ratings (FC.list contentRatingSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "stardateFrom" stardateFrom FC.number
      #+ FC.optionalField FC.OmitKey_DelegateNull "genres" genres (FC.list genreSchema)
      #+ FC.required "title" title FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "references" references (FC.list referenceSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "yearTo" yearTo FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "developers" developers (FC.list companyBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "platforms" platforms (FC.list platformSchema)