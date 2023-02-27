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
      #+ FC.optional "titleJapanese" titleJapanese FC.text
      #+ FC.optional "directors" directors (FC.list staffBaseSchema)
      #+ FC.optional "yearFrom" yearFrom FC.integer
      #+ FC.optional "stardateTo" stardateTo FC.number
      #+ FC.optional "standInPerformers" standInPerformers (FC.list performerBaseSchema)
      #+ FC.optional "titleItalian" titleItalian FC.text
      #+ FC.optional "performers" performers (FC.list performerBaseSchema)
      #+ FC.optional "titleBulgarian" titleBulgarian FC.text
      #+ FC.optional "usReleaseDate" usReleaseDate FC.text
      #+ FC.optional "stuntPerformers" stuntPerformers (FC.list performerBaseSchema)
      #+ FC.optional "titlePolish" titlePolish FC.text
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "storyAuthors" storyAuthors (FC.list staffBaseSchema)
      #+ FC.optional "stardateFrom" stardateFrom FC.number
      #+ FC.optional "titleSpanish" titleSpanish FC.text
      #+ FC.optional "characters" characters (FC.list characterBaseSchema)
      #+ FC.optional "titleCatalan" titleCatalan FC.text
      #+ FC.optional "titleRussian" titleRussian FC.text
      #+ FC.optional "titleGerman" titleGerman FC.text
      #+ FC.required "title" title FC.text
      #+ FC.optional "producers" producers (FC.list staffBaseSchema)
      #+ FC.optional "yearTo" yearTo FC.integer
      #+ FC.optional "staff" staff (FC.list staffBaseSchema)
      #+ FC.optional "titleSerbian" titleSerbian FC.text
      #+ FC.optional "titleChineseTraditional" titleChineseTraditional FC.text
      #+ FC.optional "screenplayAuthors" screenplayAuthors (FC.list staffBaseSchema)
      #+ FC.optional "writers" writers (FC.list staffBaseSchema)
      #+ FC.optional "mainDirector" mainDirector staffBaseSchema