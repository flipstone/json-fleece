{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MovieFull
  ( MovieFull(..)
  , movieFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.CharacterBase (CharacterBase, characterBaseSchema)
import StarTrek.MovieFull.StardateFrom (StardateFrom, stardateFromSchema)
import StarTrek.MovieFull.StardateTo (StardateTo, stardateToSchema)
import StarTrek.MovieFull.Title (Title, titleSchema)
import StarTrek.MovieFull.TitleBulgarian (TitleBulgarian, titleBulgarianSchema)
import StarTrek.MovieFull.TitleCatalan (TitleCatalan, titleCatalanSchema)
import StarTrek.MovieFull.TitleChineseTraditional (TitleChineseTraditional, titleChineseTraditionalSchema)
import StarTrek.MovieFull.TitleGerman (TitleGerman, titleGermanSchema)
import StarTrek.MovieFull.TitleItalian (TitleItalian, titleItalianSchema)
import StarTrek.MovieFull.TitleJapanese (TitleJapanese, titleJapaneseSchema)
import StarTrek.MovieFull.TitlePolish (TitlePolish, titlePolishSchema)
import StarTrek.MovieFull.TitleRussian (TitleRussian, titleRussianSchema)
import StarTrek.MovieFull.TitleSerbian (TitleSerbian, titleSerbianSchema)
import StarTrek.MovieFull.TitleSpanish (TitleSpanish, titleSpanishSchema)
import StarTrek.MovieFull.Uid (Uid, uidSchema)
import StarTrek.MovieFull.UsReleaseDate (UsReleaseDate, usReleaseDateSchema)
import StarTrek.MovieFull.YearFrom (YearFrom, yearFromSchema)
import StarTrek.MovieFull.YearTo (YearTo, yearToSchema)
import StarTrek.PerformerBase (PerformerBase, performerBaseSchema)
import StarTrek.StaffBase (StaffBase, staffBaseSchema)

data MovieFull = MovieFull
  { titleJapanese :: Maybe TitleJapanese -- ^ Movie title in Japanese
  , directors :: Maybe [StaffBase] -- ^ Base staff, returned in search results
  , yearFrom :: Maybe YearFrom -- ^ Starting year of movie story
  , stardateTo :: Maybe StardateTo -- ^ Ending stardate of movie story
  , standInPerformers :: Maybe [PerformerBase] -- ^ Base performer, returned in search results
  , titleItalian :: Maybe TitleItalian -- ^ Movie title in Italian
  , performers :: Maybe [PerformerBase] -- ^ Base performer, returned in search results
  , titleBulgarian :: Maybe TitleBulgarian -- ^ Movie title in Bulgarian
  , usReleaseDate :: Maybe UsReleaseDate -- ^ Date the movie was first released in the United States
  , stuntPerformers :: Maybe [PerformerBase] -- ^ Base performer, returned in search results
  , titlePolish :: Maybe TitlePolish -- ^ Movie title in Polish
  , uid :: Uid -- ^ Movie unique ID
  , storyAuthors :: Maybe [StaffBase] -- ^ Base staff, returned in search results
  , stardateFrom :: Maybe StardateFrom -- ^ Starting stardate of movie story
  , titleSpanish :: Maybe TitleSpanish -- ^ Movie title in Spanish
  , characters :: Maybe [CharacterBase] -- ^ Base character, returned in search results
  , titleCatalan :: Maybe TitleCatalan -- ^ Movie title in Catalan
  , titleRussian :: Maybe TitleRussian -- ^ Movie title in Russian
  , titleGerman :: Maybe TitleGerman -- ^ Movie title in German
  , title :: Title -- ^ Movie title
  , producers :: Maybe [StaffBase] -- ^ Base staff, returned in search results
  , yearTo :: Maybe YearTo -- ^ Ending year of movie story
  , staff :: Maybe [StaffBase] -- ^ Base staff, returned in search results
  , titleSerbian :: Maybe TitleSerbian -- ^ Movie title in Serbian
  , titleChineseTraditional :: Maybe TitleChineseTraditional -- ^ Movie title in Chinese traditional
  , screenplayAuthors :: Maybe [StaffBase] -- ^ Base staff, returned in search results
  , writers :: Maybe [StaffBase] -- ^ Base staff, returned in search results
  , mainDirector :: Maybe StaffBase -- ^ Base staff, returned in search results
  }
  deriving (Eq, Show)

movieFullSchema :: FC.Fleece schema => schema MovieFull
movieFullSchema =
  FC.object $
    FC.constructor MovieFull
      #+ FC.optional "titleJapanese" titleJapanese titleJapaneseSchema
      #+ FC.optional "directors" directors (FC.list staffBaseSchema)
      #+ FC.optional "yearFrom" yearFrom yearFromSchema
      #+ FC.optional "stardateTo" stardateTo stardateToSchema
      #+ FC.optional "standInPerformers" standInPerformers (FC.list performerBaseSchema)
      #+ FC.optional "titleItalian" titleItalian titleItalianSchema
      #+ FC.optional "performers" performers (FC.list performerBaseSchema)
      #+ FC.optional "titleBulgarian" titleBulgarian titleBulgarianSchema
      #+ FC.optional "usReleaseDate" usReleaseDate usReleaseDateSchema
      #+ FC.optional "stuntPerformers" stuntPerformers (FC.list performerBaseSchema)
      #+ FC.optional "titlePolish" titlePolish titlePolishSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "storyAuthors" storyAuthors (FC.list staffBaseSchema)
      #+ FC.optional "stardateFrom" stardateFrom stardateFromSchema
      #+ FC.optional "titleSpanish" titleSpanish titleSpanishSchema
      #+ FC.optional "characters" characters (FC.list characterBaseSchema)
      #+ FC.optional "titleCatalan" titleCatalan titleCatalanSchema
      #+ FC.optional "titleRussian" titleRussian titleRussianSchema
      #+ FC.optional "titleGerman" titleGerman titleGermanSchema
      #+ FC.required "title" title titleSchema
      #+ FC.optional "producers" producers (FC.list staffBaseSchema)
      #+ FC.optional "yearTo" yearTo yearToSchema
      #+ FC.optional "staff" staff (FC.list staffBaseSchema)
      #+ FC.optional "titleSerbian" titleSerbian titleSerbianSchema
      #+ FC.optional "titleChineseTraditional" titleChineseTraditional titleChineseTraditionalSchema
      #+ FC.optional "screenplayAuthors" screenplayAuthors (FC.list staffBaseSchema)
      #+ FC.optional "writers" writers (FC.list staffBaseSchema)
      #+ FC.optional "mainDirector" mainDirector staffBaseSchema