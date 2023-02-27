{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoReleaseBase
  ( VideoReleaseBase(..)
  , videoReleaseBaseSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Bool, Eq, Integer, Maybe, Show)
import StarTrek.SeasonHeader (SeasonHeader, seasonHeaderSchema)
import StarTrek.SeriesHeader (SeriesHeader, seriesHeaderSchema)
import StarTrek.VideoReleaseFormat (VideoReleaseFormat, videoReleaseFormatSchema)

data VideoReleaseBase = VideoReleaseBase
  { youTubeDigitalRelease :: Maybe Bool -- ^ Whether this video has been release on YouTube
  , numberOfFeatureLengthEpisodes :: Maybe Integer -- ^ Number of feature-length episodes
  , yearFrom :: Maybe Integer -- ^ Starting year of video release story
  , iTunesDigitalRelease :: Maybe Bool -- ^ Whether this video has been release on iTunes
  , format :: Maybe VideoReleaseFormat -- ^ Video release format
  , dailymotionDigitalRelease :: Maybe Bool -- ^ Whether this video has been release on Dailymotion
  , region1SlimlineReleaseDate :: Maybe Text -- ^ Region 1 slimline release date
  , vuduDigitalRelease :: Maybe Bool -- ^ Whether this video has been release on VUDU
  , amazonDigitalRelease :: Maybe Bool -- ^ Whether this video has been release on Amazon.com
  , region4AReleaseDate :: Maybe Text -- ^ Region 4 release date
  , runTime :: Maybe Integer -- ^ Run time, in minutes
  , uid :: Text -- ^ Video release unique ID
  , vimeoDigitalRelease :: Maybe Bool -- ^ Whether this video has been release on Vimeo
  , region2SlimlineReleaseDate :: Maybe Text -- ^ Region 2 slimline release date
  , googlePlayDigitalRelease :: Maybe Bool -- ^ Whether this video has been release on Google Play
  , title :: Text -- ^ Video release title
  , region1AReleaseDate :: Maybe Text -- ^ Region 1/A release date
  , xboxSmartGlassDigitalRelease :: Maybe Bool -- ^ Whether this video has been release on Xbox SmartGlass
  , yearTo :: Maybe Integer -- ^ Ending year of video release story
  , ultraVioletDigitalRelease :: Maybe Bool -- ^ Whether this video has been release on UltraViolet
  , series :: Maybe SeriesHeader -- ^ Header series, embedded in other objects
  , region4SlimlineReleaseDate :: Maybe Text -- ^ Region 4 slimline release date
  , season :: Maybe SeasonHeader -- ^ Header season, embedded in other objects
  , region2BReleaseDate :: Maybe Text -- ^ Region 2/B release date
  , netflixDigitalRelease :: Maybe Bool -- ^ Whether this video has been release on Netflix
  , numberOfDataCarriers :: Maybe Integer -- ^ Number of data carriers (like DVD, VCD, VHS etc.)
  , regionFreeReleaseDate :: Maybe Text -- ^ Region free release date
  , numberOfEpisodes :: Maybe Integer -- ^ Number of episodes
  }
  deriving (Eq, Show)

videoReleaseBaseSchema :: FC.Fleece schema => schema VideoReleaseBase
videoReleaseBaseSchema =
  FC.object $
    FC.constructor VideoReleaseBase
      #+ FC.optional "youTubeDigitalRelease" youTubeDigitalRelease FC.boolean
      #+ FC.optional "numberOfFeatureLengthEpisodes" numberOfFeatureLengthEpisodes FC.integer
      #+ FC.optional "yearFrom" yearFrom FC.integer
      #+ FC.optional "iTunesDigitalRelease" iTunesDigitalRelease FC.boolean
      #+ FC.optional "format" format videoReleaseFormatSchema
      #+ FC.optional "dailymotionDigitalRelease" dailymotionDigitalRelease FC.boolean
      #+ FC.optional "region1SlimlineReleaseDate" region1SlimlineReleaseDate FC.text
      #+ FC.optional "vuduDigitalRelease" vuduDigitalRelease FC.boolean
      #+ FC.optional "amazonDigitalRelease" amazonDigitalRelease FC.boolean
      #+ FC.optional "region4AReleaseDate" region4AReleaseDate FC.text
      #+ FC.optional "runTime" runTime FC.integer
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "vimeoDigitalRelease" vimeoDigitalRelease FC.boolean
      #+ FC.optional "region2SlimlineReleaseDate" region2SlimlineReleaseDate FC.text
      #+ FC.optional "googlePlayDigitalRelease" googlePlayDigitalRelease FC.boolean
      #+ FC.required "title" title FC.text
      #+ FC.optional "region1AReleaseDate" region1AReleaseDate FC.text
      #+ FC.optional "xboxSmartGlassDigitalRelease" xboxSmartGlassDigitalRelease FC.boolean
      #+ FC.optional "yearTo" yearTo FC.integer
      #+ FC.optional "ultraVioletDigitalRelease" ultraVioletDigitalRelease FC.boolean
      #+ FC.optional "series" series seriesHeaderSchema
      #+ FC.optional "region4SlimlineReleaseDate" region4SlimlineReleaseDate FC.text
      #+ FC.optional "season" season seasonHeaderSchema
      #+ FC.optional "region2BReleaseDate" region2BReleaseDate FC.text
      #+ FC.optional "netflixDigitalRelease" netflixDigitalRelease FC.boolean
      #+ FC.optional "numberOfDataCarriers" numberOfDataCarriers FC.integer
      #+ FC.optional "regionFreeReleaseDate" regionFreeReleaseDate FC.text
      #+ FC.optional "numberOfEpisodes" numberOfEpisodes FC.integer