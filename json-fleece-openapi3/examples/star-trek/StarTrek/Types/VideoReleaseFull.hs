{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.VideoReleaseFull
  ( VideoReleaseFull(..)
  , videoReleaseFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.ContentLanguage as ContentLanguage
import qualified StarTrek.Types.ContentRating as ContentRating
import qualified StarTrek.Types.Reference as Reference
import qualified StarTrek.Types.SeasonBase as SeasonBase
import qualified StarTrek.Types.SeriesBase as SeriesBase
import qualified StarTrek.Types.VideoReleaseFormat as VideoReleaseFormat
import qualified StarTrek.Types.VideoReleaseFull.AmazonDigitalRelease as AmazonDigitalRelease
import qualified StarTrek.Types.VideoReleaseFull.DailymotionDigitalRelease as DailymotionDigitalRelease
import qualified StarTrek.Types.VideoReleaseFull.GooglePlayDigitalRelease as GooglePlayDigitalRelease
import qualified StarTrek.Types.VideoReleaseFull.ITunesDigitalRelease as ITunesDigitalRelease
import qualified StarTrek.Types.VideoReleaseFull.NetflixDigitalRelease as NetflixDigitalRelease
import qualified StarTrek.Types.VideoReleaseFull.NumberOfDataCarriers as NumberOfDataCarriers
import qualified StarTrek.Types.VideoReleaseFull.NumberOfEpisodes as NumberOfEpisodes
import qualified StarTrek.Types.VideoReleaseFull.NumberOfFeatureLengthEpisodes as NumberOfFeatureLengthEpisodes
import qualified StarTrek.Types.VideoReleaseFull.Region1AReleaseDate as Region1AReleaseDate
import qualified StarTrek.Types.VideoReleaseFull.Region1SlimlineReleaseDate as Region1SlimlineReleaseDate
import qualified StarTrek.Types.VideoReleaseFull.Region2BReleaseDate as Region2BReleaseDate
import qualified StarTrek.Types.VideoReleaseFull.Region2SlimlineReleaseDate as Region2SlimlineReleaseDate
import qualified StarTrek.Types.VideoReleaseFull.Region4AReleaseDate as Region4AReleaseDate
import qualified StarTrek.Types.VideoReleaseFull.Region4SlimlineReleaseDate as Region4SlimlineReleaseDate
import qualified StarTrek.Types.VideoReleaseFull.RegionFreeReleaseDate as RegionFreeReleaseDate
import qualified StarTrek.Types.VideoReleaseFull.RunTime as RunTime
import qualified StarTrek.Types.VideoReleaseFull.Title as Title
import qualified StarTrek.Types.VideoReleaseFull.Uid as Uid
import qualified StarTrek.Types.VideoReleaseFull.UltraVioletDigitalRelease as UltraVioletDigitalRelease
import qualified StarTrek.Types.VideoReleaseFull.VimeoDigitalRelease as VimeoDigitalRelease
import qualified StarTrek.Types.VideoReleaseFull.VuduDigitalRelease as VuduDigitalRelease
import qualified StarTrek.Types.VideoReleaseFull.XboxSmartGlassDigitalRelease as XboxSmartGlassDigitalRelease
import qualified StarTrek.Types.VideoReleaseFull.YearFrom as YearFrom
import qualified StarTrek.Types.VideoReleaseFull.YearTo as YearTo
import qualified StarTrek.Types.VideoReleaseFull.YouTubeDigitalRelease as YouTubeDigitalRelease

data VideoReleaseFull = VideoReleaseFull
  { region2SlimlineReleaseDate :: Maybe Region2SlimlineReleaseDate.Region2SlimlineReleaseDate -- ^ Region 2 slimline release date
  , numberOfDataCarriers :: Maybe NumberOfDataCarriers.NumberOfDataCarriers -- ^ Number of data carriers (like DVD, VCD, VHS etc.)
  , amazonDigitalRelease :: Maybe AmazonDigitalRelease.AmazonDigitalRelease -- ^ Whether this video has been release on Amazon.com
  , format :: Maybe VideoReleaseFormat.VideoReleaseFormat -- ^ Video release format
  , title :: Title.Title -- ^ Video release title
  , yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of video release story
  , numberOfEpisodes :: Maybe NumberOfEpisodes.NumberOfEpisodes -- ^ Number of episodes
  , languagesDubbed :: Maybe [ContentLanguage.ContentLanguage] -- ^ Rating of video release, etc.
  , region4SlimlineReleaseDate :: Maybe Region4SlimlineReleaseDate.Region4SlimlineReleaseDate -- ^ Region 4 slimline release date
  , googlePlayDigitalRelease :: Maybe GooglePlayDigitalRelease.GooglePlayDigitalRelease -- ^ Whether this video has been release on Google Play
  , runTime :: Maybe RunTime.RunTime -- ^ Run time, in minutes
  , dailymotionDigitalRelease :: Maybe DailymotionDigitalRelease.DailymotionDigitalRelease -- ^ Whether this video has been release on Dailymotion
  , vuduDigitalRelease :: Maybe VuduDigitalRelease.VuduDigitalRelease -- ^ Whether this video has been release on VUDU
  , season :: Maybe SeasonBase.SeasonBase -- ^ Base season, returned in search results
  , iTunesDigitalRelease :: Maybe ITunesDigitalRelease.ITunesDigitalRelease -- ^ Whether this video has been release on iTunes
  , youTubeDigitalRelease :: Maybe YouTubeDigitalRelease.YouTubeDigitalRelease -- ^ Whether this video has been release on YouTube
  , region2BReleaseDate :: Maybe Region2BReleaseDate.Region2BReleaseDate -- ^ Region 2/B release date
  , languages :: Maybe [ContentLanguage.ContentLanguage] -- ^ Rating of video release, etc.
  , netflixDigitalRelease :: Maybe NetflixDigitalRelease.NetflixDigitalRelease -- ^ Whether this video has been release on Netflix
  , languagesSubtitles :: Maybe [ContentLanguage.ContentLanguage] -- ^ Rating of video release, etc.
  , uid :: Uid.Uid -- ^ Video release unique ID
  , numberOfFeatureLengthEpisodes :: Maybe NumberOfFeatureLengthEpisodes.NumberOfFeatureLengthEpisodes -- ^ Number of feature-length episodes
  , vimeoDigitalRelease :: Maybe VimeoDigitalRelease.VimeoDigitalRelease -- ^ Whether this video has been release on Vimeo
  , references :: Maybe [Reference.Reference] -- ^ Reference of book, comics, video release, etc.
  , xboxSmartGlassDigitalRelease :: Maybe XboxSmartGlassDigitalRelease.XboxSmartGlassDigitalRelease -- ^ Whether this video has been release on Xbox SmartGlass
  , ultraVioletDigitalRelease :: Maybe UltraVioletDigitalRelease.UltraVioletDigitalRelease -- ^ Whether this video has been release on UltraViolet
  , ratings :: Maybe [ContentRating.ContentRating] -- ^ Rating of video release, etc.
  , regionFreeReleaseDate :: Maybe RegionFreeReleaseDate.RegionFreeReleaseDate -- ^ Region free release date
  , region4AReleaseDate :: Maybe Region4AReleaseDate.Region4AReleaseDate -- ^ Region 4 release date
  , region1AReleaseDate :: Maybe Region1AReleaseDate.Region1AReleaseDate -- ^ Region 1/A release date
  , yearTo :: Maybe YearTo.YearTo -- ^ Ending year of video release story
  , region1SlimlineReleaseDate :: Maybe Region1SlimlineReleaseDate.Region1SlimlineReleaseDate -- ^ Region 1 slimline release date
  , series :: Maybe SeriesBase.SeriesBase -- ^ Base series, returned in search results
  }
  deriving (Eq, Show)

videoReleaseFullSchema :: FC.Fleece schema => schema VideoReleaseFull
videoReleaseFullSchema =
  FC.object $
    FC.constructor VideoReleaseFull
      #+ FC.optional "region2SlimlineReleaseDate" region2SlimlineReleaseDate Region2SlimlineReleaseDate.region2SlimlineReleaseDateSchema
      #+ FC.optional "numberOfDataCarriers" numberOfDataCarriers NumberOfDataCarriers.numberOfDataCarriersSchema
      #+ FC.optional "amazonDigitalRelease" amazonDigitalRelease AmazonDigitalRelease.amazonDigitalReleaseSchema
      #+ FC.optional "format" format VideoReleaseFormat.videoReleaseFormatSchema
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.optional "numberOfEpisodes" numberOfEpisodes NumberOfEpisodes.numberOfEpisodesSchema
      #+ FC.optional "languagesDubbed" languagesDubbed (FC.list ContentLanguage.contentLanguageSchema)
      #+ FC.optional "region4SlimlineReleaseDate" region4SlimlineReleaseDate Region4SlimlineReleaseDate.region4SlimlineReleaseDateSchema
      #+ FC.optional "googlePlayDigitalRelease" googlePlayDigitalRelease GooglePlayDigitalRelease.googlePlayDigitalReleaseSchema
      #+ FC.optional "runTime" runTime RunTime.runTimeSchema
      #+ FC.optional "dailymotionDigitalRelease" dailymotionDigitalRelease DailymotionDigitalRelease.dailymotionDigitalReleaseSchema
      #+ FC.optional "vuduDigitalRelease" vuduDigitalRelease VuduDigitalRelease.vuduDigitalReleaseSchema
      #+ FC.optional "season" season SeasonBase.seasonBaseSchema
      #+ FC.optional "iTunesDigitalRelease" iTunesDigitalRelease ITunesDigitalRelease.iTunesDigitalReleaseSchema
      #+ FC.optional "youTubeDigitalRelease" youTubeDigitalRelease YouTubeDigitalRelease.youTubeDigitalReleaseSchema
      #+ FC.optional "region2BReleaseDate" region2BReleaseDate Region2BReleaseDate.region2BReleaseDateSchema
      #+ FC.optional "languages" languages (FC.list ContentLanguage.contentLanguageSchema)
      #+ FC.optional "netflixDigitalRelease" netflixDigitalRelease NetflixDigitalRelease.netflixDigitalReleaseSchema
      #+ FC.optional "languagesSubtitles" languagesSubtitles (FC.list ContentLanguage.contentLanguageSchema)
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "numberOfFeatureLengthEpisodes" numberOfFeatureLengthEpisodes NumberOfFeatureLengthEpisodes.numberOfFeatureLengthEpisodesSchema
      #+ FC.optional "vimeoDigitalRelease" vimeoDigitalRelease VimeoDigitalRelease.vimeoDigitalReleaseSchema
      #+ FC.optional "references" references (FC.list Reference.referenceSchema)
      #+ FC.optional "xboxSmartGlassDigitalRelease" xboxSmartGlassDigitalRelease XboxSmartGlassDigitalRelease.xboxSmartGlassDigitalReleaseSchema
      #+ FC.optional "ultraVioletDigitalRelease" ultraVioletDigitalRelease UltraVioletDigitalRelease.ultraVioletDigitalReleaseSchema
      #+ FC.optional "ratings" ratings (FC.list ContentRating.contentRatingSchema)
      #+ FC.optional "regionFreeReleaseDate" regionFreeReleaseDate RegionFreeReleaseDate.regionFreeReleaseDateSchema
      #+ FC.optional "region4AReleaseDate" region4AReleaseDate Region4AReleaseDate.region4AReleaseDateSchema
      #+ FC.optional "region1AReleaseDate" region1AReleaseDate Region1AReleaseDate.region1AReleaseDateSchema
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema
      #+ FC.optional "region1SlimlineReleaseDate" region1SlimlineReleaseDate Region1SlimlineReleaseDate.region1SlimlineReleaseDateSchema
      #+ FC.optional "series" series SeriesBase.seriesBaseSchema