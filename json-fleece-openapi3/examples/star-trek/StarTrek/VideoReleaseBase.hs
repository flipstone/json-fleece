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
      #+ FC.optionalField FC.OmitKey_DelegateNull "youTubeDigitalRelease" youTubeDigitalRelease FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "numberOfFeatureLengthEpisodes" numberOfFeatureLengthEpisodes FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "yearFrom" yearFrom FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "iTunesDigitalRelease" iTunesDigitalRelease FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "format" format videoReleaseFormatSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "dailymotionDigitalRelease" dailymotionDigitalRelease FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "region1SlimlineReleaseDate" region1SlimlineReleaseDate FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "vuduDigitalRelease" vuduDigitalRelease FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "amazonDigitalRelease" amazonDigitalRelease FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "region4AReleaseDate" region4AReleaseDate FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "runTime" runTime FC.integer
      #+ FC.required "uid" uid FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "vimeoDigitalRelease" vimeoDigitalRelease FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "region2SlimlineReleaseDate" region2SlimlineReleaseDate FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "googlePlayDigitalRelease" googlePlayDigitalRelease FC.boolean
      #+ FC.required "title" title FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "region1AReleaseDate" region1AReleaseDate FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "xboxSmartGlassDigitalRelease" xboxSmartGlassDigitalRelease FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "yearTo" yearTo FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "ultraVioletDigitalRelease" ultraVioletDigitalRelease FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "series" series seriesHeaderSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "region4SlimlineReleaseDate" region4SlimlineReleaseDate FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "season" season seasonHeaderSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "region2BReleaseDate" region2BReleaseDate FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "netflixDigitalRelease" netflixDigitalRelease FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "numberOfDataCarriers" numberOfDataCarriers FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "regionFreeReleaseDate" regionFreeReleaseDate FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "numberOfEpisodes" numberOfEpisodes FC.integer