{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoReleaseFull
  ( VideoReleaseFull(..)
  , videoReleaseFullSchema
  ) where

import Data.Text (Text)
import Data.Time (Day)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Bool, Eq, Integer, Maybe, Show)
import StarTrek.ContentLanguage (ContentLanguage, contentLanguageSchema)
import StarTrek.ContentRating (ContentRating, contentRatingSchema)
import StarTrek.Reference (Reference, referenceSchema)
import StarTrek.SeasonBase (SeasonBase, seasonBaseSchema)
import StarTrek.SeriesBase (SeriesBase, seriesBaseSchema)
import StarTrek.VideoReleaseFormat (VideoReleaseFormat, videoReleaseFormatSchema)

data VideoReleaseFull = VideoReleaseFull
  { youTubeDigitalRelease :: Maybe Bool -- ^ Whether this video has been release on YouTube
  , numberOfFeatureLengthEpisodes :: Maybe Integer -- ^ Number of feature-length episodes
  , languagesSubtitles :: Maybe [ContentLanguage] -- ^ Languages of subtitles
  , yearFrom :: Maybe Integer -- ^ Starting year of video release story
  , iTunesDigitalRelease :: Maybe Bool -- ^ Whether this video has been release on iTunes
  , format :: Maybe VideoReleaseFormat -- ^ Video release format
  , dailymotionDigitalRelease :: Maybe Bool -- ^ Whether this video has been release on Dailymotion
  , region1SlimlineReleaseDate :: Maybe Day -- ^ Region 1 slimline release date
  , vuduDigitalRelease :: Maybe Bool -- ^ Whether this video has been release on VUDU
  , languagesDubbed :: Maybe [ContentLanguage] -- ^ Languages that are available with dubbing
  , amazonDigitalRelease :: Maybe Bool -- ^ Whether this video has been release on Amazon.com
  , region4AReleaseDate :: Maybe Day -- ^ Region 4 release date
  , runTime :: Maybe Integer -- ^ Run time, in minutes
  , uid :: Text -- ^ Video release unique ID
  , ratings :: Maybe [ContentRating] -- ^ Ratings
  , vimeoDigitalRelease :: Maybe Bool -- ^ Whether this video has been release on Vimeo
  , region2SlimlineReleaseDate :: Maybe Day -- ^ Region 2 slimline release date
  , googlePlayDigitalRelease :: Maybe Bool -- ^ Whether this video has been release on Google Play
  , title :: Text -- ^ Video release title
  , region1AReleaseDate :: Maybe Day -- ^ Region 1/A release date
  , xboxSmartGlassDigitalRelease :: Maybe Bool -- ^ Whether this video has been release on Xbox SmartGlass
  , references :: Maybe [Reference] -- ^ References
  , yearTo :: Maybe Integer -- ^ Ending year of video release story
  , ultraVioletDigitalRelease :: Maybe Bool -- ^ Whether this video has been release on UltraViolet
  , series :: Maybe SeriesBase -- ^ Base series, returned in search results
  , region4SlimlineReleaseDate :: Maybe Day -- ^ Region 4 slimline release date
  , languages :: Maybe [ContentLanguage] -- ^ Languages of audio track
  , season :: Maybe SeasonBase -- ^ Base season, returned in search results
  , region2BReleaseDate :: Maybe Day -- ^ Region 2/B release date
  , netflixDigitalRelease :: Maybe Bool -- ^ Whether this video has been release on Netflix
  , numberOfDataCarriers :: Maybe Integer -- ^ Number of data carriers (like DVD, VCD, VHS etc.)
  , regionFreeReleaseDate :: Maybe Day -- ^ Region free release date
  , numberOfEpisodes :: Maybe Integer -- ^ Number of episodes
  }
  deriving (Eq, Show)

videoReleaseFullSchema :: FC.Fleece schema => schema VideoReleaseFull
videoReleaseFullSchema =
  FC.object $
    FC.constructor VideoReleaseFull
      #+ FC.optional "youTubeDigitalRelease" youTubeDigitalRelease FC.boolean
      #+ FC.optional "numberOfFeatureLengthEpisodes" numberOfFeatureLengthEpisodes FC.integer
      #+ FC.optional "languagesSubtitles" languagesSubtitles (FC.list contentLanguageSchema)
      #+ FC.optional "yearFrom" yearFrom FC.integer
      #+ FC.optional "iTunesDigitalRelease" iTunesDigitalRelease FC.boolean
      #+ FC.optional "format" format videoReleaseFormatSchema
      #+ FC.optional "dailymotionDigitalRelease" dailymotionDigitalRelease FC.boolean
      #+ FC.optional "region1SlimlineReleaseDate" region1SlimlineReleaseDate FC.day
      #+ FC.optional "vuduDigitalRelease" vuduDigitalRelease FC.boolean
      #+ FC.optional "languagesDubbed" languagesDubbed (FC.list contentLanguageSchema)
      #+ FC.optional "amazonDigitalRelease" amazonDigitalRelease FC.boolean
      #+ FC.optional "region4AReleaseDate" region4AReleaseDate FC.day
      #+ FC.optional "runTime" runTime FC.integer
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "ratings" ratings (FC.list contentRatingSchema)
      #+ FC.optional "vimeoDigitalRelease" vimeoDigitalRelease FC.boolean
      #+ FC.optional "region2SlimlineReleaseDate" region2SlimlineReleaseDate FC.day
      #+ FC.optional "googlePlayDigitalRelease" googlePlayDigitalRelease FC.boolean
      #+ FC.required "title" title FC.text
      #+ FC.optional "region1AReleaseDate" region1AReleaseDate FC.day
      #+ FC.optional "xboxSmartGlassDigitalRelease" xboxSmartGlassDigitalRelease FC.boolean
      #+ FC.optional "references" references (FC.list referenceSchema)
      #+ FC.optional "yearTo" yearTo FC.integer
      #+ FC.optional "ultraVioletDigitalRelease" ultraVioletDigitalRelease FC.boolean
      #+ FC.optional "series" series seriesBaseSchema
      #+ FC.optional "region4SlimlineReleaseDate" region4SlimlineReleaseDate FC.day
      #+ FC.optional "languages" languages (FC.list contentLanguageSchema)
      #+ FC.optional "season" season seasonBaseSchema
      #+ FC.optional "region2BReleaseDate" region2BReleaseDate FC.day
      #+ FC.optional "netflixDigitalRelease" netflixDigitalRelease FC.boolean
      #+ FC.optional "numberOfDataCarriers" numberOfDataCarriers FC.integer
      #+ FC.optional "regionFreeReleaseDate" regionFreeReleaseDate FC.day
      #+ FC.optional "numberOfEpisodes" numberOfEpisodes FC.integer