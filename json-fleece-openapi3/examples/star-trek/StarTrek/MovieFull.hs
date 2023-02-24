{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MovieFull
  ( MovieFull(..)
  , movieFullSchema
  ) where

import qualified Fleece.Core as FC
import Data.Scientific (Scientific)
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Integer, Maybe, Show)
import StarTrek.CharacterBase (CharacterBase, characterBaseSchema)
import StarTrek.PerformerBase (PerformerBase, performerBaseSchema)
import StarTrek.StaffBase (StaffBase, staffBaseSchema)

data MovieFull = MovieFull
  { titleJapanese :: Maybe Text -- ^ Movie title in Japanese
  , directors :: Maybe [StaffBase] -- ^ Directors authors involved in the movie
  , yearFrom :: Maybe Integer -- ^ Starting year of movie story
  , stardateTo :: Maybe Scientific -- ^ Ending stardate of movie story
  , standInPerformers :: Maybe [PerformerBase] -- ^ Stand-in performers appearing in the movie
  , titleItalian :: Maybe Text -- ^ Movie title in Italian
  , performers :: Maybe [PerformerBase] -- ^ Performers appearing in the movie
  , titleBulgarian :: Maybe Text -- ^ Movie title in Bulgarian
  , usReleaseDate :: Maybe Text -- ^ Date the movie was first released in the United States
  , stuntPerformers :: Maybe [PerformerBase] -- ^ Stunt performers appearing in the movie
  , titlePolish :: Maybe Text -- ^ Movie title in Polish
  , uid :: Text -- ^ Movie unique ID
  , storyAuthors :: Maybe [StaffBase] -- ^ Story authors authors involved in the movie
  , stardateFrom :: Maybe Scientific -- ^ Starting stardate of movie story
  , titleSpanish :: Maybe Text -- ^ Movie title in Spanish
  , characters :: Maybe [CharacterBase] -- ^ Characters appearing in the movie
  , titleCatalan :: Maybe Text -- ^ Movie title in Catalan
  , titleRussian :: Maybe Text -- ^ Movie title in Russian
  , titleGerman :: Maybe Text -- ^ Movie title in German
  , title :: Text -- ^ Movie title
  , producers :: Maybe [StaffBase] -- ^ Producers authors involved in the movie
  , yearTo :: Maybe Integer -- ^ Ending year of movie story
  , staff :: Maybe [StaffBase] -- ^ Other staff involved in the movie
  , titleSerbian :: Maybe Text -- ^ Movie title in Serbian
  , titleChineseTraditional :: Maybe Text -- ^ Movie title in Chinese traditional
  , screenplayAuthors :: Maybe [StaffBase] -- ^ Screenplay authors involved in the movie
  , writers :: Maybe [StaffBase] -- ^ Writers involved in the movie
  , mainDirector :: Maybe StaffBase -- ^ Base staff, returned in search results
  }
  deriving (Eq, Show)

movieFullSchema :: FC.Fleece schema => schema MovieFull
movieFullSchema =
  FC.object $
    FC.constructor MovieFull
      #+ FC.optionalField FC.OmitKey_DelegateNull "titleJapanese" titleJapanese FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "directors" directors (FC.list staffBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "yearFrom" yearFrom FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "stardateTo" stardateTo FC.number
      #+ FC.optionalField FC.OmitKey_DelegateNull "standInPerformers" standInPerformers (FC.list performerBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "titleItalian" titleItalian FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "performers" performers (FC.list performerBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "titleBulgarian" titleBulgarian FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "usReleaseDate" usReleaseDate FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "stuntPerformers" stuntPerformers (FC.list performerBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "titlePolish" titlePolish FC.text
      #+ FC.required "uid" uid FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "storyAuthors" storyAuthors (FC.list staffBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "stardateFrom" stardateFrom FC.number
      #+ FC.optionalField FC.OmitKey_DelegateNull "titleSpanish" titleSpanish FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "characters" characters (FC.list characterBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "titleCatalan" titleCatalan FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "titleRussian" titleRussian FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "titleGerman" titleGerman FC.text
      #+ FC.required "title" title FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "producers" producers (FC.list staffBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "yearTo" yearTo FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "staff" staff (FC.list staffBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "titleSerbian" titleSerbian FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "titleChineseTraditional" titleChineseTraditional FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "screenplayAuthors" screenplayAuthors (FC.list staffBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "writers" writers (FC.list staffBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "mainDirector" mainDirector staffBaseSchema