{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MovieBase
  ( MovieBase(..)
  , movieBaseSchema
  ) where

import Data.Text (Text)
import Data.Time (Day)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Float, Integer, Maybe, Show)
import StarTrek.StaffHeader (StaffHeader, staffHeaderSchema)

data MovieBase = MovieBase
  { titleJapanese :: Maybe Text -- ^ Movie title in Japanese
  , yearFrom :: Maybe Integer -- ^ Starting year of movie story
  , stardateTo :: Maybe Float -- ^ Ending stardate of movie story
  , titleItalian :: Maybe Text -- ^ Movie title in Italian
  , titleBulgarian :: Maybe Text -- ^ Movie title in Bulgarian
  , usReleaseDate :: Maybe Day -- ^ Date the movie was first released in the United States
  , titlePolish :: Maybe Text -- ^ Movie title in Polish
  , uid :: Text -- ^ Movie unique ID
  , stardateFrom :: Maybe Float -- ^ Starting stardate of movie story
  , titleSpanish :: Maybe Text -- ^ Movie title in Spanish
  , titleCatalan :: Maybe Text -- ^ Movie title in Catalan
  , titleRussian :: Maybe Text -- ^ Movie title in Russian
  , titleGerman :: Maybe Text -- ^ Movie title in German
  , title :: Text -- ^ Movie title
  , yearTo :: Maybe Integer -- ^ Ending year of movie story
  , titleSerbian :: Maybe Text -- ^ Movie title in Serbian
  , titleChineseTraditional :: Maybe Text -- ^ Movie title in Chinese traditional
  , mainDirector :: Maybe StaffHeader -- ^ Header staff, embedded in other objects
  }
  deriving (Eq, Show)

movieBaseSchema :: FC.Fleece schema => schema MovieBase
movieBaseSchema =
  FC.object $
    FC.constructor MovieBase
      #+ FC.optional "titleJapanese" titleJapanese FC.text
      #+ FC.optional "yearFrom" yearFrom FC.integer
      #+ FC.optional "stardateTo" stardateTo FC.float
      #+ FC.optional "titleItalian" titleItalian FC.text
      #+ FC.optional "titleBulgarian" titleBulgarian FC.text
      #+ FC.optional "usReleaseDate" usReleaseDate FC.day
      #+ FC.optional "titlePolish" titlePolish FC.text
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "stardateFrom" stardateFrom FC.float
      #+ FC.optional "titleSpanish" titleSpanish FC.text
      #+ FC.optional "titleCatalan" titleCatalan FC.text
      #+ FC.optional "titleRussian" titleRussian FC.text
      #+ FC.optional "titleGerman" titleGerman FC.text
      #+ FC.required "title" title FC.text
      #+ FC.optional "yearTo" yearTo FC.integer
      #+ FC.optional "titleSerbian" titleSerbian FC.text
      #+ FC.optional "titleChineseTraditional" titleChineseTraditional FC.text
      #+ FC.optional "mainDirector" mainDirector staffHeaderSchema