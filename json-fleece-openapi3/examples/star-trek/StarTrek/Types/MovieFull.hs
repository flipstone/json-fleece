{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MovieFull
  ( MovieFull(..)
  , movieFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.CharacterBase as CharacterBase
import qualified StarTrek.Types.MovieFull.StardateFrom as StardateFrom
import qualified StarTrek.Types.MovieFull.StardateTo as StardateTo
import qualified StarTrek.Types.MovieFull.Title as Title
import qualified StarTrek.Types.MovieFull.TitleBulgarian as TitleBulgarian
import qualified StarTrek.Types.MovieFull.TitleCatalan as TitleCatalan
import qualified StarTrek.Types.MovieFull.TitleChineseTraditional as TitleChineseTraditional
import qualified StarTrek.Types.MovieFull.TitleGerman as TitleGerman
import qualified StarTrek.Types.MovieFull.TitleItalian as TitleItalian
import qualified StarTrek.Types.MovieFull.TitleJapanese as TitleJapanese
import qualified StarTrek.Types.MovieFull.TitlePolish as TitlePolish
import qualified StarTrek.Types.MovieFull.TitleRussian as TitleRussian
import qualified StarTrek.Types.MovieFull.TitleSerbian as TitleSerbian
import qualified StarTrek.Types.MovieFull.TitleSpanish as TitleSpanish
import qualified StarTrek.Types.MovieFull.Uid as Uid
import qualified StarTrek.Types.MovieFull.UsReleaseDate as UsReleaseDate
import qualified StarTrek.Types.MovieFull.YearFrom as YearFrom
import qualified StarTrek.Types.MovieFull.YearTo as YearTo
import qualified StarTrek.Types.PerformerBase as PerformerBase
import qualified StarTrek.Types.StaffBase as StaffBase

data MovieFull = MovieFull
  { titleJapanese :: Maybe TitleJapanese.TitleJapanese -- ^ Movie title in Japanese
  , directors :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of movie story
  , stardateTo :: Maybe StardateTo.StardateTo -- ^ Ending stardate of movie story
  , standInPerformers :: Maybe [PerformerBase.PerformerBase] -- ^ Base performer, returned in search results
  , titleItalian :: Maybe TitleItalian.TitleItalian -- ^ Movie title in Italian
  , performers :: Maybe [PerformerBase.PerformerBase] -- ^ Base performer, returned in search results
  , titleBulgarian :: Maybe TitleBulgarian.TitleBulgarian -- ^ Movie title in Bulgarian
  , usReleaseDate :: Maybe UsReleaseDate.UsReleaseDate -- ^ Date the movie was first released in the United States
  , stuntPerformers :: Maybe [PerformerBase.PerformerBase] -- ^ Base performer, returned in search results
  , titlePolish :: Maybe TitlePolish.TitlePolish -- ^ Movie title in Polish
  , uid :: Uid.Uid -- ^ Movie unique ID
  , storyAuthors :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , stardateFrom :: Maybe StardateFrom.StardateFrom -- ^ Starting stardate of movie story
  , titleSpanish :: Maybe TitleSpanish.TitleSpanish -- ^ Movie title in Spanish
  , characters :: Maybe [CharacterBase.CharacterBase] -- ^ Base character, returned in search results
  , titleCatalan :: Maybe TitleCatalan.TitleCatalan -- ^ Movie title in Catalan
  , titleRussian :: Maybe TitleRussian.TitleRussian -- ^ Movie title in Russian
  , titleGerman :: Maybe TitleGerman.TitleGerman -- ^ Movie title in German
  , title :: Title.Title -- ^ Movie title
  , producers :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , yearTo :: Maybe YearTo.YearTo -- ^ Ending year of movie story
  , staff :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , titleSerbian :: Maybe TitleSerbian.TitleSerbian -- ^ Movie title in Serbian
  , titleChineseTraditional :: Maybe TitleChineseTraditional.TitleChineseTraditional -- ^ Movie title in Chinese traditional
  , screenplayAuthors :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , writers :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , mainDirector :: Maybe StaffBase.StaffBase -- ^ Base staff, returned in search results
  }
  deriving (Eq, Show)

movieFullSchema :: FC.Fleece schema => schema MovieFull
movieFullSchema =
  FC.object $
    FC.constructor MovieFull
      #+ FC.optional "titleJapanese" titleJapanese TitleJapanese.titleJapaneseSchema
      #+ FC.optional "directors" directors (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.optional "stardateTo" stardateTo StardateTo.stardateToSchema
      #+ FC.optional "standInPerformers" standInPerformers (FC.list PerformerBase.performerBaseSchema)
      #+ FC.optional "titleItalian" titleItalian TitleItalian.titleItalianSchema
      #+ FC.optional "performers" performers (FC.list PerformerBase.performerBaseSchema)
      #+ FC.optional "titleBulgarian" titleBulgarian TitleBulgarian.titleBulgarianSchema
      #+ FC.optional "usReleaseDate" usReleaseDate UsReleaseDate.usReleaseDateSchema
      #+ FC.optional "stuntPerformers" stuntPerformers (FC.list PerformerBase.performerBaseSchema)
      #+ FC.optional "titlePolish" titlePolish TitlePolish.titlePolishSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "storyAuthors" storyAuthors (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "stardateFrom" stardateFrom StardateFrom.stardateFromSchema
      #+ FC.optional "titleSpanish" titleSpanish TitleSpanish.titleSpanishSchema
      #+ FC.optional "characters" characters (FC.list CharacterBase.characterBaseSchema)
      #+ FC.optional "titleCatalan" titleCatalan TitleCatalan.titleCatalanSchema
      #+ FC.optional "titleRussian" titleRussian TitleRussian.titleRussianSchema
      #+ FC.optional "titleGerman" titleGerman TitleGerman.titleGermanSchema
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "producers" producers (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema
      #+ FC.optional "staff" staff (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "titleSerbian" titleSerbian TitleSerbian.titleSerbianSchema
      #+ FC.optional "titleChineseTraditional" titleChineseTraditional TitleChineseTraditional.titleChineseTraditionalSchema
      #+ FC.optional "screenplayAuthors" screenplayAuthors (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "writers" writers (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "mainDirector" mainDirector StaffBase.staffBaseSchema