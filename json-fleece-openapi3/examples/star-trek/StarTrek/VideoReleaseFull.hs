{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoReleaseFull
  ( VideoReleaseFull(..)
  , videoReleaseFullSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
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
  , region1SlimlineReleaseDate :: Maybe Text -- ^ Region 1 slimline release date
  , vuduDigitalRelease :: Maybe Bool -- ^ Whether this video has been release on VUDU
  , languagesDubbed :: Maybe [ContentLanguage] -- ^ Languages that are available with dubbing
  , amazonDigitalRelease :: Maybe Bool -- ^ Whether this video has been release on Amazon.com
  , region4AReleaseDate :: Maybe Text -- ^ Region 4 release date
  , runTime :: Maybe Integer -- ^ Run time, in minutes
  , uid :: Text -- ^ Video release unique ID
  , ratings :: Maybe [ContentRating] -- ^ Ratings
  , vimeoDigitalRelease :: Maybe Bool -- ^ Whether this video has been release on Vimeo
  , region2SlimlineReleaseDate :: Maybe Text -- ^ Region 2 slimline release date
  , googlePlayDigitalRelease :: Maybe Bool -- ^ Whether this video has been release on Google Play
  , title :: Text -- ^ Video release title
  , region1AReleaseDate :: Maybe Text -- ^ Region 1/A release date
  , xboxSmartGlassDigitalRelease :: Maybe Bool -- ^ Whether this video has been release on Xbox SmartGlass
  , references :: Maybe [Reference] -- ^ References
  , yearTo :: Maybe Integer -- ^ Ending year of video release story
  , ultraVioletDigitalRelease :: Maybe Bool -- ^ Whether this video has been release on UltraViolet
  , series :: Maybe SeriesBase -- ^ Base series, returned in search results
  , region4SlimlineReleaseDate :: Maybe Text -- ^ Region 4 slimline release date
  , languages :: Maybe [ContentLanguage] -- ^ Languages of audio track
  , season :: Maybe SeasonBase -- ^ Base season, returned in search results
  , region2BReleaseDate :: Maybe Text -- ^ Region 2/B release date
  , netflixDigitalRelease :: Maybe Bool -- ^ Whether this video has been release on Netflix
  , numberOfDataCarriers :: Maybe Integer -- ^ Number of data carriers (like DVD, VCD, VHS etc.)
  , regionFreeReleaseDate :: Maybe Text -- ^ Region free release date
  , numberOfEpisodes :: Maybe Integer -- ^ Number of episodes
  }
  deriving (Eq, Show)

videoReleaseFullSchema :: FC.Fleece schema => schema VideoReleaseFull
videoReleaseFullSchema =
  FC.object $
    FC.constructor VideoReleaseFull
      #+ FC.optionalField FC.OmitKey_DelegateNull "youTubeDigitalRelease" youTubeDigitalRelease FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "numberOfFeatureLengthEpisodes" numberOfFeatureLengthEpisodes FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "languagesSubtitles" languagesSubtitles (FC.list contentLanguageSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "yearFrom" yearFrom FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "iTunesDigitalRelease" iTunesDigitalRelease FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "format" format videoReleaseFormatSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "dailymotionDigitalRelease" dailymotionDigitalRelease FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "region1SlimlineReleaseDate" region1SlimlineReleaseDate FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "vuduDigitalRelease" vuduDigitalRelease FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "languagesDubbed" languagesDubbed (FC.list contentLanguageSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "amazonDigitalRelease" amazonDigitalRelease FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "region4AReleaseDate" region4AReleaseDate FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "runTime" runTime FC.integer
      #+ FC.required "uid" uid FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "ratings" ratings (FC.list contentRatingSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "vimeoDigitalRelease" vimeoDigitalRelease FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "region2SlimlineReleaseDate" region2SlimlineReleaseDate FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "googlePlayDigitalRelease" googlePlayDigitalRelease FC.boolean
      #+ FC.required "title" title FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "region1AReleaseDate" region1AReleaseDate FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "xboxSmartGlassDigitalRelease" xboxSmartGlassDigitalRelease FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "references" references (FC.list referenceSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "yearTo" yearTo FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "ultraVioletDigitalRelease" ultraVioletDigitalRelease FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "series" series seriesBaseSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "region4SlimlineReleaseDate" region4SlimlineReleaseDate FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "languages" languages (FC.list contentLanguageSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "season" season seasonBaseSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "region2BReleaseDate" region2BReleaseDate FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "netflixDigitalRelease" netflixDigitalRelease FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "numberOfDataCarriers" numberOfDataCarriers FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "regionFreeReleaseDate" regionFreeReleaseDate FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "numberOfEpisodes" numberOfEpisodes FC.integer