{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MovieBase
  ( MovieBase(..)
  , movieBaseSchema
  ) where

import qualified Fleece.Core as FC
import Data.Scientific (Scientific)
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Integer, Maybe, Show)
import StarTrek.StaffHeader (StaffHeader, staffHeaderSchema)

data MovieBase = MovieBase
  { titleJapanese :: Maybe Text -- ^ Movie title in Japanese
  , yearFrom :: Maybe Integer -- ^ Starting year of movie story
  , stardateTo :: Maybe Scientific -- ^ Ending stardate of movie story
  , titleItalian :: Maybe Text -- ^ Movie title in Italian
  , titleBulgarian :: Maybe Text -- ^ Movie title in Bulgarian
  , usReleaseDate :: Maybe Text -- ^ Date the movie was first released in the United States
  , titlePolish :: Maybe Text -- ^ Movie title in Polish
  , uid :: Text -- ^ Movie unique ID
  , stardateFrom :: Maybe Scientific -- ^ Starting stardate of movie story
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
      #+ FC.optionalField FC.OmitKey_DelegateNull "titleJapanese" titleJapanese FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "yearFrom" yearFrom FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "stardateTo" stardateTo FC.number
      #+ FC.optionalField FC.OmitKey_DelegateNull "titleItalian" titleItalian FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "titleBulgarian" titleBulgarian FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "usReleaseDate" usReleaseDate FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "titlePolish" titlePolish FC.text
      #+ FC.required "uid" uid FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "stardateFrom" stardateFrom FC.number
      #+ FC.optionalField FC.OmitKey_DelegateNull "titleSpanish" titleSpanish FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "titleCatalan" titleCatalan FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "titleRussian" titleRussian FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "titleGerman" titleGerman FC.text
      #+ FC.required "title" title FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "yearTo" yearTo FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "titleSerbian" titleSerbian FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "titleChineseTraditional" titleChineseTraditional FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "mainDirector" mainDirector staffHeaderSchema