{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoReleaseBase
  ( VideoReleaseBase(..)
  , videoReleaseBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.SeasonHeader (SeasonHeader, seasonHeaderSchema)
import StarTrek.SeriesHeader (SeriesHeader, seriesHeaderSchema)
import StarTrek.VideoReleaseBase.AmazonDigitalRelease (AmazonDigitalRelease, amazonDigitalReleaseSchema)
import StarTrek.VideoReleaseBase.DailymotionDigitalRelease (DailymotionDigitalRelease, dailymotionDigitalReleaseSchema)
import StarTrek.VideoReleaseBase.GooglePlayDigitalRelease (GooglePlayDigitalRelease, googlePlayDigitalReleaseSchema)
import StarTrek.VideoReleaseBase.ITunesDigitalRelease (ITunesDigitalRelease, iTunesDigitalReleaseSchema)
import StarTrek.VideoReleaseBase.NetflixDigitalRelease (NetflixDigitalRelease, netflixDigitalReleaseSchema)
import StarTrek.VideoReleaseBase.NumberOfDataCarriers (NumberOfDataCarriers, numberOfDataCarriersSchema)
import StarTrek.VideoReleaseBase.NumberOfEpisodes (NumberOfEpisodes, numberOfEpisodesSchema)
import StarTrek.VideoReleaseBase.NumberOfFeatureLengthEpisodes (NumberOfFeatureLengthEpisodes, numberOfFeatureLengthEpisodesSchema)
import StarTrek.VideoReleaseBase.Region1AReleaseDate (Region1AReleaseDate, region1AReleaseDateSchema)
import StarTrek.VideoReleaseBase.Region1SlimlineReleaseDate (Region1SlimlineReleaseDate, region1SlimlineReleaseDateSchema)
import StarTrek.VideoReleaseBase.Region2BReleaseDate (Region2BReleaseDate, region2BReleaseDateSchema)
import StarTrek.VideoReleaseBase.Region2SlimlineReleaseDate (Region2SlimlineReleaseDate, region2SlimlineReleaseDateSchema)
import StarTrek.VideoReleaseBase.Region4AReleaseDate (Region4AReleaseDate, region4AReleaseDateSchema)
import StarTrek.VideoReleaseBase.Region4SlimlineReleaseDate (Region4SlimlineReleaseDate, region4SlimlineReleaseDateSchema)
import StarTrek.VideoReleaseBase.RegionFreeReleaseDate (RegionFreeReleaseDate, regionFreeReleaseDateSchema)
import StarTrek.VideoReleaseBase.RunTime (RunTime, runTimeSchema)
import StarTrek.VideoReleaseBase.Title (Title, titleSchema)
import StarTrek.VideoReleaseBase.Uid (Uid, uidSchema)
import StarTrek.VideoReleaseBase.UltraVioletDigitalRelease (UltraVioletDigitalRelease, ultraVioletDigitalReleaseSchema)
import StarTrek.VideoReleaseBase.VimeoDigitalRelease (VimeoDigitalRelease, vimeoDigitalReleaseSchema)
import StarTrek.VideoReleaseBase.VuduDigitalRelease (VuduDigitalRelease, vuduDigitalReleaseSchema)
import StarTrek.VideoReleaseBase.XboxSmartGlassDigitalRelease (XboxSmartGlassDigitalRelease, xboxSmartGlassDigitalReleaseSchema)
import StarTrek.VideoReleaseBase.YearFrom (YearFrom, yearFromSchema)
import StarTrek.VideoReleaseBase.YearTo (YearTo, yearToSchema)
import StarTrek.VideoReleaseBase.YouTubeDigitalRelease (YouTubeDigitalRelease, youTubeDigitalReleaseSchema)
import StarTrek.VideoReleaseFormat (VideoReleaseFormat, videoReleaseFormatSchema)

data VideoReleaseBase = VideoReleaseBase
  { youTubeDigitalRelease :: Maybe YouTubeDigitalRelease -- ^ Whether this video has been release on YouTube
  , numberOfFeatureLengthEpisodes :: Maybe NumberOfFeatureLengthEpisodes -- ^ Number of feature-length episodes
  , yearFrom :: Maybe YearFrom -- ^ Starting year of video release story
  , iTunesDigitalRelease :: Maybe ITunesDigitalRelease -- ^ Whether this video has been release on iTunes
  , format :: Maybe VideoReleaseFormat -- ^ Video release format
  , dailymotionDigitalRelease :: Maybe DailymotionDigitalRelease -- ^ Whether this video has been release on Dailymotion
  , region1SlimlineReleaseDate :: Maybe Region1SlimlineReleaseDate -- ^ Region 1 slimline release date
  , vuduDigitalRelease :: Maybe VuduDigitalRelease -- ^ Whether this video has been release on VUDU
  , amazonDigitalRelease :: Maybe AmazonDigitalRelease -- ^ Whether this video has been release on Amazon.com
  , region4AReleaseDate :: Maybe Region4AReleaseDate -- ^ Region 4 release date
  , runTime :: Maybe RunTime -- ^ Run time, in minutes
  , uid :: Uid -- ^ Video release unique ID
  , vimeoDigitalRelease :: Maybe VimeoDigitalRelease -- ^ Whether this video has been release on Vimeo
  , region2SlimlineReleaseDate :: Maybe Region2SlimlineReleaseDate -- ^ Region 2 slimline release date
  , googlePlayDigitalRelease :: Maybe GooglePlayDigitalRelease -- ^ Whether this video has been release on Google Play
  , title :: Title -- ^ Video release title
  , region1AReleaseDate :: Maybe Region1AReleaseDate -- ^ Region 1/A release date
  , xboxSmartGlassDigitalRelease :: Maybe XboxSmartGlassDigitalRelease -- ^ Whether this video has been release on Xbox SmartGlass
  , yearTo :: Maybe YearTo -- ^ Ending year of video release story
  , ultraVioletDigitalRelease :: Maybe UltraVioletDigitalRelease -- ^ Whether this video has been release on UltraViolet
  , series :: Maybe SeriesHeader -- ^ Header series, embedded in other objects
  , region4SlimlineReleaseDate :: Maybe Region4SlimlineReleaseDate -- ^ Region 4 slimline release date
  , season :: Maybe SeasonHeader -- ^ Header season, embedded in other objects
  , region2BReleaseDate :: Maybe Region2BReleaseDate -- ^ Region 2/B release date
  , netflixDigitalRelease :: Maybe NetflixDigitalRelease -- ^ Whether this video has been release on Netflix
  , numberOfDataCarriers :: Maybe NumberOfDataCarriers -- ^ Number of data carriers (like DVD, VCD, VHS etc.)
  , regionFreeReleaseDate :: Maybe RegionFreeReleaseDate -- ^ Region free release date
  , numberOfEpisodes :: Maybe NumberOfEpisodes -- ^ Number of episodes
  }
  deriving (Eq, Show)

videoReleaseBaseSchema :: FC.Fleece schema => schema VideoReleaseBase
videoReleaseBaseSchema =
  FC.object $
    FC.constructor VideoReleaseBase
      #+ FC.optional "youTubeDigitalRelease" youTubeDigitalRelease youTubeDigitalReleaseSchema
      #+ FC.optional "numberOfFeatureLengthEpisodes" numberOfFeatureLengthEpisodes numberOfFeatureLengthEpisodesSchema
      #+ FC.optional "yearFrom" yearFrom yearFromSchema
      #+ FC.optional "iTunesDigitalRelease" iTunesDigitalRelease iTunesDigitalReleaseSchema
      #+ FC.optional "format" format videoReleaseFormatSchema
      #+ FC.optional "dailymotionDigitalRelease" dailymotionDigitalRelease dailymotionDigitalReleaseSchema
      #+ FC.optional "region1SlimlineReleaseDate" region1SlimlineReleaseDate region1SlimlineReleaseDateSchema
      #+ FC.optional "vuduDigitalRelease" vuduDigitalRelease vuduDigitalReleaseSchema
      #+ FC.optional "amazonDigitalRelease" amazonDigitalRelease amazonDigitalReleaseSchema
      #+ FC.optional "region4AReleaseDate" region4AReleaseDate region4AReleaseDateSchema
      #+ FC.optional "runTime" runTime runTimeSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "vimeoDigitalRelease" vimeoDigitalRelease vimeoDigitalReleaseSchema
      #+ FC.optional "region2SlimlineReleaseDate" region2SlimlineReleaseDate region2SlimlineReleaseDateSchema
      #+ FC.optional "googlePlayDigitalRelease" googlePlayDigitalRelease googlePlayDigitalReleaseSchema
      #+ FC.required "title" title titleSchema
      #+ FC.optional "region1AReleaseDate" region1AReleaseDate region1AReleaseDateSchema
      #+ FC.optional "xboxSmartGlassDigitalRelease" xboxSmartGlassDigitalRelease xboxSmartGlassDigitalReleaseSchema
      #+ FC.optional "yearTo" yearTo yearToSchema
      #+ FC.optional "ultraVioletDigitalRelease" ultraVioletDigitalRelease ultraVioletDigitalReleaseSchema
      #+ FC.optional "series" series seriesHeaderSchema
      #+ FC.optional "region4SlimlineReleaseDate" region4SlimlineReleaseDate region4SlimlineReleaseDateSchema
      #+ FC.optional "season" season seasonHeaderSchema
      #+ FC.optional "region2BReleaseDate" region2BReleaseDate region2BReleaseDateSchema
      #+ FC.optional "netflixDigitalRelease" netflixDigitalRelease netflixDigitalReleaseSchema
      #+ FC.optional "numberOfDataCarriers" numberOfDataCarriers numberOfDataCarriersSchema
      #+ FC.optional "regionFreeReleaseDate" regionFreeReleaseDate regionFreeReleaseDateSchema
      #+ FC.optional "numberOfEpisodes" numberOfEpisodes numberOfEpisodesSchema