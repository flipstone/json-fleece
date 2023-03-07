{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MovieBase
  ( MovieBase(..)
  , movieBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.MovieBase.StardateFrom as StardateFrom
import qualified StarTrek.Types.MovieBase.StardateTo as StardateTo
import qualified StarTrek.Types.MovieBase.Title as Title
import qualified StarTrek.Types.MovieBase.TitleBulgarian as TitleBulgarian
import qualified StarTrek.Types.MovieBase.TitleCatalan as TitleCatalan
import qualified StarTrek.Types.MovieBase.TitleChineseTraditional as TitleChineseTraditional
import qualified StarTrek.Types.MovieBase.TitleGerman as TitleGerman
import qualified StarTrek.Types.MovieBase.TitleItalian as TitleItalian
import qualified StarTrek.Types.MovieBase.TitleJapanese as TitleJapanese
import qualified StarTrek.Types.MovieBase.TitlePolish as TitlePolish
import qualified StarTrek.Types.MovieBase.TitleRussian as TitleRussian
import qualified StarTrek.Types.MovieBase.TitleSerbian as TitleSerbian
import qualified StarTrek.Types.MovieBase.TitleSpanish as TitleSpanish
import qualified StarTrek.Types.MovieBase.Uid as Uid
import qualified StarTrek.Types.MovieBase.UsReleaseDate as UsReleaseDate
import qualified StarTrek.Types.MovieBase.YearFrom as YearFrom
import qualified StarTrek.Types.MovieBase.YearTo as YearTo
import qualified StarTrek.Types.StaffHeader as StaffHeader

data MovieBase = MovieBase
  { titleJapanese :: Maybe TitleJapanese.TitleJapanese -- ^ Movie title in Japanese
  , yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of movie story
  , stardateTo :: Maybe StardateTo.StardateTo -- ^ Ending stardate of movie story
  , titleItalian :: Maybe TitleItalian.TitleItalian -- ^ Movie title in Italian
  , titleBulgarian :: Maybe TitleBulgarian.TitleBulgarian -- ^ Movie title in Bulgarian
  , usReleaseDate :: Maybe UsReleaseDate.UsReleaseDate -- ^ Date the movie was first released in the United States
  , titlePolish :: Maybe TitlePolish.TitlePolish -- ^ Movie title in Polish
  , uid :: Uid.Uid -- ^ Movie unique ID
  , stardateFrom :: Maybe StardateFrom.StardateFrom -- ^ Starting stardate of movie story
  , titleSpanish :: Maybe TitleSpanish.TitleSpanish -- ^ Movie title in Spanish
  , titleCatalan :: Maybe TitleCatalan.TitleCatalan -- ^ Movie title in Catalan
  , titleRussian :: Maybe TitleRussian.TitleRussian -- ^ Movie title in Russian
  , titleGerman :: Maybe TitleGerman.TitleGerman -- ^ Movie title in German
  , title :: Title.Title -- ^ Movie title
  , yearTo :: Maybe YearTo.YearTo -- ^ Ending year of movie story
  , titleSerbian :: Maybe TitleSerbian.TitleSerbian -- ^ Movie title in Serbian
  , titleChineseTraditional :: Maybe TitleChineseTraditional.TitleChineseTraditional -- ^ Movie title in Chinese traditional
  , mainDirector :: Maybe StaffHeader.StaffHeader -- ^ Header staff, embedded in other objects
  }
  deriving (Eq, Show)

movieBaseSchema :: FC.Fleece schema => schema MovieBase
movieBaseSchema =
  FC.object $
    FC.constructor MovieBase
      #+ FC.optional "titleJapanese" titleJapanese TitleJapanese.titleJapaneseSchema
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.optional "stardateTo" stardateTo StardateTo.stardateToSchema
      #+ FC.optional "titleItalian" titleItalian TitleItalian.titleItalianSchema
      #+ FC.optional "titleBulgarian" titleBulgarian TitleBulgarian.titleBulgarianSchema
      #+ FC.optional "usReleaseDate" usReleaseDate UsReleaseDate.usReleaseDateSchema
      #+ FC.optional "titlePolish" titlePolish TitlePolish.titlePolishSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "stardateFrom" stardateFrom StardateFrom.stardateFromSchema
      #+ FC.optional "titleSpanish" titleSpanish TitleSpanish.titleSpanishSchema
      #+ FC.optional "titleCatalan" titleCatalan TitleCatalan.titleCatalanSchema
      #+ FC.optional "titleRussian" titleRussian TitleRussian.titleRussianSchema
      #+ FC.optional "titleGerman" titleGerman TitleGerman.titleGermanSchema
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema
      #+ FC.optional "titleSerbian" titleSerbian TitleSerbian.titleSerbianSchema
      #+ FC.optional "titleChineseTraditional" titleChineseTraditional TitleChineseTraditional.titleChineseTraditionalSchema
      #+ FC.optional "mainDirector" mainDirector StaffHeader.staffHeaderSchema