{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.VideoReleaseBase
  ( VideoReleaseBase(..)
  , videoReleaseBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.SeasonHeader as SeasonHeader
import qualified StarTrek.Types.SeriesHeader as SeriesHeader
import qualified StarTrek.Types.VideoReleaseBase.AmazonDigitalRelease as AmazonDigitalRelease
import qualified StarTrek.Types.VideoReleaseBase.DailymotionDigitalRelease as DailymotionDigitalRelease
import qualified StarTrek.Types.VideoReleaseBase.GooglePlayDigitalRelease as GooglePlayDigitalRelease
import qualified StarTrek.Types.VideoReleaseBase.ITunesDigitalRelease as ITunesDigitalRelease
import qualified StarTrek.Types.VideoReleaseBase.NetflixDigitalRelease as NetflixDigitalRelease
import qualified StarTrek.Types.VideoReleaseBase.NumberOfDataCarriers as NumberOfDataCarriers
import qualified StarTrek.Types.VideoReleaseBase.NumberOfEpisodes as NumberOfEpisodes
import qualified StarTrek.Types.VideoReleaseBase.NumberOfFeatureLengthEpisodes as NumberOfFeatureLengthEpisodes
import qualified StarTrek.Types.VideoReleaseBase.Region1AReleaseDate as Region1AReleaseDate
import qualified StarTrek.Types.VideoReleaseBase.Region1SlimlineReleaseDate as Region1SlimlineReleaseDate
import qualified StarTrek.Types.VideoReleaseBase.Region2BReleaseDate as Region2BReleaseDate
import qualified StarTrek.Types.VideoReleaseBase.Region2SlimlineReleaseDate as Region2SlimlineReleaseDate
import qualified StarTrek.Types.VideoReleaseBase.Region4AReleaseDate as Region4AReleaseDate
import qualified StarTrek.Types.VideoReleaseBase.Region4SlimlineReleaseDate as Region4SlimlineReleaseDate
import qualified StarTrek.Types.VideoReleaseBase.RegionFreeReleaseDate as RegionFreeReleaseDate
import qualified StarTrek.Types.VideoReleaseBase.RunTime as RunTime
import qualified StarTrek.Types.VideoReleaseBase.Title as Title
import qualified StarTrek.Types.VideoReleaseBase.Uid as Uid
import qualified StarTrek.Types.VideoReleaseBase.UltraVioletDigitalRelease as UltraVioletDigitalRelease
import qualified StarTrek.Types.VideoReleaseBase.VimeoDigitalRelease as VimeoDigitalRelease
import qualified StarTrek.Types.VideoReleaseBase.VuduDigitalRelease as VuduDigitalRelease
import qualified StarTrek.Types.VideoReleaseBase.XboxSmartGlassDigitalRelease as XboxSmartGlassDigitalRelease
import qualified StarTrek.Types.VideoReleaseBase.YearFrom as YearFrom
import qualified StarTrek.Types.VideoReleaseBase.YearTo as YearTo
import qualified StarTrek.Types.VideoReleaseBase.YouTubeDigitalRelease as YouTubeDigitalRelease
import qualified StarTrek.Types.VideoReleaseFormat as VideoReleaseFormat

data VideoReleaseBase = VideoReleaseBase
  { amazonDigitalRelease :: Maybe AmazonDigitalRelease.AmazonDigitalRelease -- ^ Whether this video has been release on Amazon.com
  , dailymotionDigitalRelease :: Maybe DailymotionDigitalRelease.DailymotionDigitalRelease -- ^ Whether this video has been release on Dailymotion
  , format :: Maybe VideoReleaseFormat.VideoReleaseFormat -- ^ Video release format
  , googlePlayDigitalRelease :: Maybe GooglePlayDigitalRelease.GooglePlayDigitalRelease -- ^ Whether this video has been release on Google Play
  , iTunesDigitalRelease :: Maybe ITunesDigitalRelease.ITunesDigitalRelease -- ^ Whether this video has been release on iTunes
  , netflixDigitalRelease :: Maybe NetflixDigitalRelease.NetflixDigitalRelease -- ^ Whether this video has been release on Netflix
  , numberOfDataCarriers :: Maybe NumberOfDataCarriers.NumberOfDataCarriers -- ^ Number of data carriers (like DVD, VCD, VHS etc.)
  , numberOfEpisodes :: Maybe NumberOfEpisodes.NumberOfEpisodes -- ^ Number of episodes
  , numberOfFeatureLengthEpisodes :: Maybe NumberOfFeatureLengthEpisodes.NumberOfFeatureLengthEpisodes -- ^ Number of feature-length episodes
  , region1AReleaseDate :: Maybe Region1AReleaseDate.Region1AReleaseDate -- ^ Region 1/A release date
  , region1SlimlineReleaseDate :: Maybe Region1SlimlineReleaseDate.Region1SlimlineReleaseDate -- ^ Region 1 slimline release date
  , region2BReleaseDate :: Maybe Region2BReleaseDate.Region2BReleaseDate -- ^ Region 2/B release date
  , region2SlimlineReleaseDate :: Maybe Region2SlimlineReleaseDate.Region2SlimlineReleaseDate -- ^ Region 2 slimline release date
  , region4AReleaseDate :: Maybe Region4AReleaseDate.Region4AReleaseDate -- ^ Region 4 release date
  , region4SlimlineReleaseDate :: Maybe Region4SlimlineReleaseDate.Region4SlimlineReleaseDate -- ^ Region 4 slimline release date
  , regionFreeReleaseDate :: Maybe RegionFreeReleaseDate.RegionFreeReleaseDate -- ^ Region free release date
  , runTime :: Maybe RunTime.RunTime -- ^ Run time, in minutes
  , season :: Maybe SeasonHeader.SeasonHeader -- ^ Header season, embedded in other objects
  , series :: Maybe SeriesHeader.SeriesHeader -- ^ Header series, embedded in other objects
  , title :: Title.Title -- ^ Video release title
  , uid :: Uid.Uid -- ^ Video release unique ID
  , ultraVioletDigitalRelease :: Maybe UltraVioletDigitalRelease.UltraVioletDigitalRelease -- ^ Whether this video has been release on UltraViolet
  , vimeoDigitalRelease :: Maybe VimeoDigitalRelease.VimeoDigitalRelease -- ^ Whether this video has been release on Vimeo
  , vuduDigitalRelease :: Maybe VuduDigitalRelease.VuduDigitalRelease -- ^ Whether this video has been release on VUDU
  , xboxSmartGlassDigitalRelease :: Maybe XboxSmartGlassDigitalRelease.XboxSmartGlassDigitalRelease -- ^ Whether this video has been release on Xbox SmartGlass
  , yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of video release story
  , yearTo :: Maybe YearTo.YearTo -- ^ Ending year of video release story
  , youTubeDigitalRelease :: Maybe YouTubeDigitalRelease.YouTubeDigitalRelease -- ^ Whether this video has been release on YouTube
  }
  deriving (Eq, Show)

videoReleaseBaseSchema :: FC.Fleece schema => schema VideoReleaseBase
videoReleaseBaseSchema =
  FC.object $
    FC.constructor VideoReleaseBase
      #+ FC.optional "amazonDigitalRelease" amazonDigitalRelease AmazonDigitalRelease.amazonDigitalReleaseSchema
      #+ FC.optional "dailymotionDigitalRelease" dailymotionDigitalRelease DailymotionDigitalRelease.dailymotionDigitalReleaseSchema
      #+ FC.optional "format" format VideoReleaseFormat.videoReleaseFormatSchema
      #+ FC.optional "googlePlayDigitalRelease" googlePlayDigitalRelease GooglePlayDigitalRelease.googlePlayDigitalReleaseSchema
      #+ FC.optional "iTunesDigitalRelease" iTunesDigitalRelease ITunesDigitalRelease.iTunesDigitalReleaseSchema
      #+ FC.optional "netflixDigitalRelease" netflixDigitalRelease NetflixDigitalRelease.netflixDigitalReleaseSchema
      #+ FC.optional "numberOfDataCarriers" numberOfDataCarriers NumberOfDataCarriers.numberOfDataCarriersSchema
      #+ FC.optional "numberOfEpisodes" numberOfEpisodes NumberOfEpisodes.numberOfEpisodesSchema
      #+ FC.optional "numberOfFeatureLengthEpisodes" numberOfFeatureLengthEpisodes NumberOfFeatureLengthEpisodes.numberOfFeatureLengthEpisodesSchema
      #+ FC.optional "region1AReleaseDate" region1AReleaseDate Region1AReleaseDate.region1AReleaseDateSchema
      #+ FC.optional "region1SlimlineReleaseDate" region1SlimlineReleaseDate Region1SlimlineReleaseDate.region1SlimlineReleaseDateSchema
      #+ FC.optional "region2BReleaseDate" region2BReleaseDate Region2BReleaseDate.region2BReleaseDateSchema
      #+ FC.optional "region2SlimlineReleaseDate" region2SlimlineReleaseDate Region2SlimlineReleaseDate.region2SlimlineReleaseDateSchema
      #+ FC.optional "region4AReleaseDate" region4AReleaseDate Region4AReleaseDate.region4AReleaseDateSchema
      #+ FC.optional "region4SlimlineReleaseDate" region4SlimlineReleaseDate Region4SlimlineReleaseDate.region4SlimlineReleaseDateSchema
      #+ FC.optional "regionFreeReleaseDate" regionFreeReleaseDate RegionFreeReleaseDate.regionFreeReleaseDateSchema
      #+ FC.optional "runTime" runTime RunTime.runTimeSchema
      #+ FC.optional "season" season SeasonHeader.seasonHeaderSchema
      #+ FC.optional "series" series SeriesHeader.seriesHeaderSchema
      #+ FC.required "title" title Title.titleSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "ultraVioletDigitalRelease" ultraVioletDigitalRelease UltraVioletDigitalRelease.ultraVioletDigitalReleaseSchema
      #+ FC.optional "vimeoDigitalRelease" vimeoDigitalRelease VimeoDigitalRelease.vimeoDigitalReleaseSchema
      #+ FC.optional "vuduDigitalRelease" vuduDigitalRelease VuduDigitalRelease.vuduDigitalReleaseSchema
      #+ FC.optional "xboxSmartGlassDigitalRelease" xboxSmartGlassDigitalRelease XboxSmartGlassDigitalRelease.xboxSmartGlassDigitalReleaseSchema
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema
      #+ FC.optional "youTubeDigitalRelease" youTubeDigitalRelease YouTubeDigitalRelease.youTubeDigitalReleaseSchema