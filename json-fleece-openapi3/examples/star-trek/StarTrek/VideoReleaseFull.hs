{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoReleaseFull
  ( VideoReleaseFull(..)
  , videoReleaseFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ContentLanguage (ContentLanguage, contentLanguageSchema)
import StarTrek.ContentRating (ContentRating, contentRatingSchema)
import StarTrek.Reference (Reference, referenceSchema)
import StarTrek.SeasonBase (SeasonBase, seasonBaseSchema)
import StarTrek.SeriesBase (SeriesBase, seriesBaseSchema)
import StarTrek.VideoReleaseFormat (VideoReleaseFormat, videoReleaseFormatSchema)
import StarTrek.VideoReleaseFull.AmazonDigitalRelease (AmazonDigitalRelease, amazonDigitalReleaseSchema)
import StarTrek.VideoReleaseFull.DailymotionDigitalRelease (DailymotionDigitalRelease, dailymotionDigitalReleaseSchema)
import StarTrek.VideoReleaseFull.GooglePlayDigitalRelease (GooglePlayDigitalRelease, googlePlayDigitalReleaseSchema)
import StarTrek.VideoReleaseFull.ITunesDigitalRelease (ITunesDigitalRelease, iTunesDigitalReleaseSchema)
import StarTrek.VideoReleaseFull.NetflixDigitalRelease (NetflixDigitalRelease, netflixDigitalReleaseSchema)
import StarTrek.VideoReleaseFull.NumberOfDataCarriers (NumberOfDataCarriers, numberOfDataCarriersSchema)
import StarTrek.VideoReleaseFull.NumberOfEpisodes (NumberOfEpisodes, numberOfEpisodesSchema)
import StarTrek.VideoReleaseFull.NumberOfFeatureLengthEpisodes (NumberOfFeatureLengthEpisodes, numberOfFeatureLengthEpisodesSchema)
import StarTrek.VideoReleaseFull.Region1AReleaseDate (Region1AReleaseDate, region1AReleaseDateSchema)
import StarTrek.VideoReleaseFull.Region1SlimlineReleaseDate (Region1SlimlineReleaseDate, region1SlimlineReleaseDateSchema)
import StarTrek.VideoReleaseFull.Region2BReleaseDate (Region2BReleaseDate, region2BReleaseDateSchema)
import StarTrek.VideoReleaseFull.Region2SlimlineReleaseDate (Region2SlimlineReleaseDate, region2SlimlineReleaseDateSchema)
import StarTrek.VideoReleaseFull.Region4AReleaseDate (Region4AReleaseDate, region4AReleaseDateSchema)
import StarTrek.VideoReleaseFull.Region4SlimlineReleaseDate (Region4SlimlineReleaseDate, region4SlimlineReleaseDateSchema)
import StarTrek.VideoReleaseFull.RegionFreeReleaseDate (RegionFreeReleaseDate, regionFreeReleaseDateSchema)
import StarTrek.VideoReleaseFull.RunTime (RunTime, runTimeSchema)
import StarTrek.VideoReleaseFull.Title (Title, titleSchema)
import StarTrek.VideoReleaseFull.Uid (Uid, uidSchema)
import StarTrek.VideoReleaseFull.UltraVioletDigitalRelease (UltraVioletDigitalRelease, ultraVioletDigitalReleaseSchema)
import StarTrek.VideoReleaseFull.VimeoDigitalRelease (VimeoDigitalRelease, vimeoDigitalReleaseSchema)
import StarTrek.VideoReleaseFull.VuduDigitalRelease (VuduDigitalRelease, vuduDigitalReleaseSchema)
import StarTrek.VideoReleaseFull.XboxSmartGlassDigitalRelease (XboxSmartGlassDigitalRelease, xboxSmartGlassDigitalReleaseSchema)
import StarTrek.VideoReleaseFull.YearFrom (YearFrom, yearFromSchema)
import StarTrek.VideoReleaseFull.YearTo (YearTo, yearToSchema)
import StarTrek.VideoReleaseFull.YouTubeDigitalRelease (YouTubeDigitalRelease, youTubeDigitalReleaseSchema)

data VideoReleaseFull = VideoReleaseFull
  { youTubeDigitalRelease :: Maybe YouTubeDigitalRelease -- ^ Whether this video has been release on YouTube
  , numberOfFeatureLengthEpisodes :: Maybe NumberOfFeatureLengthEpisodes -- ^ Number of feature-length episodes
  , languagesSubtitles :: Maybe [ContentLanguage] -- ^ Rating of video release, etc.
  , yearFrom :: Maybe YearFrom -- ^ Starting year of video release story
  , iTunesDigitalRelease :: Maybe ITunesDigitalRelease -- ^ Whether this video has been release on iTunes
  , format :: Maybe VideoReleaseFormat -- ^ Video release format
  , dailymotionDigitalRelease :: Maybe DailymotionDigitalRelease -- ^ Whether this video has been release on Dailymotion
  , region1SlimlineReleaseDate :: Maybe Region1SlimlineReleaseDate -- ^ Region 1 slimline release date
  , vuduDigitalRelease :: Maybe VuduDigitalRelease -- ^ Whether this video has been release on VUDU
  , languagesDubbed :: Maybe [ContentLanguage] -- ^ Rating of video release, etc.
  , amazonDigitalRelease :: Maybe AmazonDigitalRelease -- ^ Whether this video has been release on Amazon.com
  , region4AReleaseDate :: Maybe Region4AReleaseDate -- ^ Region 4 release date
  , runTime :: Maybe RunTime -- ^ Run time, in minutes
  , uid :: Uid -- ^ Video release unique ID
  , ratings :: Maybe [ContentRating] -- ^ Rating of video release, etc.
  , vimeoDigitalRelease :: Maybe VimeoDigitalRelease -- ^ Whether this video has been release on Vimeo
  , region2SlimlineReleaseDate :: Maybe Region2SlimlineReleaseDate -- ^ Region 2 slimline release date
  , googlePlayDigitalRelease :: Maybe GooglePlayDigitalRelease -- ^ Whether this video has been release on Google Play
  , title :: Title -- ^ Video release title
  , region1AReleaseDate :: Maybe Region1AReleaseDate -- ^ Region 1/A release date
  , xboxSmartGlassDigitalRelease :: Maybe XboxSmartGlassDigitalRelease -- ^ Whether this video has been release on Xbox SmartGlass
  , references :: Maybe [Reference] -- ^ Reference of book, comics, video release, etc.
  , yearTo :: Maybe YearTo -- ^ Ending year of video release story
  , ultraVioletDigitalRelease :: Maybe UltraVioletDigitalRelease -- ^ Whether this video has been release on UltraViolet
  , series :: Maybe SeriesBase -- ^ Base series, returned in search results
  , region4SlimlineReleaseDate :: Maybe Region4SlimlineReleaseDate -- ^ Region 4 slimline release date
  , languages :: Maybe [ContentLanguage] -- ^ Rating of video release, etc.
  , season :: Maybe SeasonBase -- ^ Base season, returned in search results
  , region2BReleaseDate :: Maybe Region2BReleaseDate -- ^ Region 2/B release date
  , netflixDigitalRelease :: Maybe NetflixDigitalRelease -- ^ Whether this video has been release on Netflix
  , numberOfDataCarriers :: Maybe NumberOfDataCarriers -- ^ Number of data carriers (like DVD, VCD, VHS etc.)
  , regionFreeReleaseDate :: Maybe RegionFreeReleaseDate -- ^ Region free release date
  , numberOfEpisodes :: Maybe NumberOfEpisodes -- ^ Number of episodes
  }
  deriving (Eq, Show)

videoReleaseFullSchema :: FC.Fleece schema => schema VideoReleaseFull
videoReleaseFullSchema =
  FC.object $
    FC.constructor VideoReleaseFull
      #+ FC.optional "youTubeDigitalRelease" youTubeDigitalRelease youTubeDigitalReleaseSchema
      #+ FC.optional "numberOfFeatureLengthEpisodes" numberOfFeatureLengthEpisodes numberOfFeatureLengthEpisodesSchema
      #+ FC.optional "languagesSubtitles" languagesSubtitles (FC.list contentLanguageSchema)
      #+ FC.optional "yearFrom" yearFrom yearFromSchema
      #+ FC.optional "iTunesDigitalRelease" iTunesDigitalRelease iTunesDigitalReleaseSchema
      #+ FC.optional "format" format videoReleaseFormatSchema
      #+ FC.optional "dailymotionDigitalRelease" dailymotionDigitalRelease dailymotionDigitalReleaseSchema
      #+ FC.optional "region1SlimlineReleaseDate" region1SlimlineReleaseDate region1SlimlineReleaseDateSchema
      #+ FC.optional "vuduDigitalRelease" vuduDigitalRelease vuduDigitalReleaseSchema
      #+ FC.optional "languagesDubbed" languagesDubbed (FC.list contentLanguageSchema)
      #+ FC.optional "amazonDigitalRelease" amazonDigitalRelease amazonDigitalReleaseSchema
      #+ FC.optional "region4AReleaseDate" region4AReleaseDate region4AReleaseDateSchema
      #+ FC.optional "runTime" runTime runTimeSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "ratings" ratings (FC.list contentRatingSchema)
      #+ FC.optional "vimeoDigitalRelease" vimeoDigitalRelease vimeoDigitalReleaseSchema
      #+ FC.optional "region2SlimlineReleaseDate" region2SlimlineReleaseDate region2SlimlineReleaseDateSchema
      #+ FC.optional "googlePlayDigitalRelease" googlePlayDigitalRelease googlePlayDigitalReleaseSchema
      #+ FC.required "title" title titleSchema
      #+ FC.optional "region1AReleaseDate" region1AReleaseDate region1AReleaseDateSchema
      #+ FC.optional "xboxSmartGlassDigitalRelease" xboxSmartGlassDigitalRelease xboxSmartGlassDigitalReleaseSchema
      #+ FC.optional "references" references (FC.list referenceSchema)
      #+ FC.optional "yearTo" yearTo yearToSchema
      #+ FC.optional "ultraVioletDigitalRelease" ultraVioletDigitalRelease ultraVioletDigitalReleaseSchema
      #+ FC.optional "series" series seriesBaseSchema
      #+ FC.optional "region4SlimlineReleaseDate" region4SlimlineReleaseDate region4SlimlineReleaseDateSchema
      #+ FC.optional "languages" languages (FC.list contentLanguageSchema)
      #+ FC.optional "season" season seasonBaseSchema
      #+ FC.optional "region2BReleaseDate" region2BReleaseDate region2BReleaseDateSchema
      #+ FC.optional "netflixDigitalRelease" netflixDigitalRelease netflixDigitalReleaseSchema
      #+ FC.optional "numberOfDataCarriers" numberOfDataCarriers numberOfDataCarriersSchema
      #+ FC.optional "regionFreeReleaseDate" regionFreeReleaseDate regionFreeReleaseDateSchema
      #+ FC.optional "numberOfEpisodes" numberOfEpisodes numberOfEpisodesSchema