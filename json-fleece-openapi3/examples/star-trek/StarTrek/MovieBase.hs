{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MovieBase
  ( MovieBase(..)
  , movieBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.MovieBase.StardateFrom (StardateFrom, stardateFromSchema)
import StarTrek.MovieBase.StardateTo (StardateTo, stardateToSchema)
import StarTrek.MovieBase.Title (Title, titleSchema)
import StarTrek.MovieBase.TitleBulgarian (TitleBulgarian, titleBulgarianSchema)
import StarTrek.MovieBase.TitleCatalan (TitleCatalan, titleCatalanSchema)
import StarTrek.MovieBase.TitleChineseTraditional (TitleChineseTraditional, titleChineseTraditionalSchema)
import StarTrek.MovieBase.TitleGerman (TitleGerman, titleGermanSchema)
import StarTrek.MovieBase.TitleItalian (TitleItalian, titleItalianSchema)
import StarTrek.MovieBase.TitleJapanese (TitleJapanese, titleJapaneseSchema)
import StarTrek.MovieBase.TitlePolish (TitlePolish, titlePolishSchema)
import StarTrek.MovieBase.TitleRussian (TitleRussian, titleRussianSchema)
import StarTrek.MovieBase.TitleSerbian (TitleSerbian, titleSerbianSchema)
import StarTrek.MovieBase.TitleSpanish (TitleSpanish, titleSpanishSchema)
import StarTrek.MovieBase.Uid (Uid, uidSchema)
import StarTrek.MovieBase.UsReleaseDate (UsReleaseDate, usReleaseDateSchema)
import StarTrek.MovieBase.YearFrom (YearFrom, yearFromSchema)
import StarTrek.MovieBase.YearTo (YearTo, yearToSchema)
import StarTrek.StaffHeader (StaffHeader, staffHeaderSchema)

data MovieBase = MovieBase
  { titleJapanese :: Maybe TitleJapanese -- ^ Movie title in Japanese
  , yearFrom :: Maybe YearFrom -- ^ Starting year of movie story
  , stardateTo :: Maybe StardateTo -- ^ Ending stardate of movie story
  , titleItalian :: Maybe TitleItalian -- ^ Movie title in Italian
  , titleBulgarian :: Maybe TitleBulgarian -- ^ Movie title in Bulgarian
  , usReleaseDate :: Maybe UsReleaseDate -- ^ Date the movie was first released in the United States
  , titlePolish :: Maybe TitlePolish -- ^ Movie title in Polish
  , uid :: Uid -- ^ Movie unique ID
  , stardateFrom :: Maybe StardateFrom -- ^ Starting stardate of movie story
  , titleSpanish :: Maybe TitleSpanish -- ^ Movie title in Spanish
  , titleCatalan :: Maybe TitleCatalan -- ^ Movie title in Catalan
  , titleRussian :: Maybe TitleRussian -- ^ Movie title in Russian
  , titleGerman :: Maybe TitleGerman -- ^ Movie title in German
  , title :: Title -- ^ Movie title
  , yearTo :: Maybe YearTo -- ^ Ending year of movie story
  , titleSerbian :: Maybe TitleSerbian -- ^ Movie title in Serbian
  , titleChineseTraditional :: Maybe TitleChineseTraditional -- ^ Movie title in Chinese traditional
  , mainDirector :: Maybe StaffHeader -- ^ Header staff, embedded in other objects
  }
  deriving (Eq, Show)

movieBaseSchema :: FC.Fleece schema => schema MovieBase
movieBaseSchema =
  FC.object $
    FC.constructor MovieBase
      #+ FC.optional "titleJapanese" titleJapanese titleJapaneseSchema
      #+ FC.optional "yearFrom" yearFrom yearFromSchema
      #+ FC.optional "stardateTo" stardateTo stardateToSchema
      #+ FC.optional "titleItalian" titleItalian titleItalianSchema
      #+ FC.optional "titleBulgarian" titleBulgarian titleBulgarianSchema
      #+ FC.optional "usReleaseDate" usReleaseDate usReleaseDateSchema
      #+ FC.optional "titlePolish" titlePolish titlePolishSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "stardateFrom" stardateFrom stardateFromSchema
      #+ FC.optional "titleSpanish" titleSpanish titleSpanishSchema
      #+ FC.optional "titleCatalan" titleCatalan titleCatalanSchema
      #+ FC.optional "titleRussian" titleRussian titleRussianSchema
      #+ FC.optional "titleGerman" titleGerman titleGermanSchema
      #+ FC.required "title" title titleSchema
      #+ FC.optional "yearTo" yearTo yearToSchema
      #+ FC.optional "titleSerbian" titleSerbian titleSerbianSchema
      #+ FC.optional "titleChineseTraditional" titleChineseTraditional titleChineseTraditionalSchema
      #+ FC.optional "mainDirector" mainDirector staffHeaderSchema