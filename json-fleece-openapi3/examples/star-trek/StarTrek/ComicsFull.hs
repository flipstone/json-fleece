{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicsFull
  ( ComicsFull(..)
  , comicsFullSchema
  ) where

import Data.Text (Text)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Bool, Eq, Float, Integer, Maybe, Show)
import StarTrek.CharacterBase (CharacterBase, characterBaseSchema)
import StarTrek.ComicCollectionBase (ComicCollectionBase, comicCollectionBaseSchema)
import StarTrek.ComicSeriesBase (ComicSeriesBase, comicSeriesBaseSchema)
import StarTrek.CompanyBase (CompanyBase, companyBaseSchema)
import StarTrek.Reference (Reference, referenceSchema)
import StarTrek.StaffBase (StaffBase, staffBaseSchema)

data ComicsFull = ComicsFull
  { comicCollections :: Maybe [ComicCollectionBase] -- ^ Comic collections this comics is included in
  , yearFrom :: Maybe Integer -- ^ Starting year of comic  story
  , stardateTo :: Maybe Float -- ^ Ending stardate of comic story
  , adaptation :: Maybe Bool -- ^ Whether it's an adaptation of an episode or a movie
  , publishers :: Maybe [CompanyBase] -- ^ Comics publishers
  , publishedMonth :: Maybe Integer -- ^ Month the comics was published
  , publishedYear :: Maybe Integer -- ^ Year the comics was published
  , uid :: Text -- ^ Comics unique ID
  , stardateFrom :: Maybe Float -- ^ Starting stardate of comic story
  , artists :: Maybe [StaffBase] -- ^ Artists involved in the comics
  , characters :: Maybe [CharacterBase] -- ^ Characters appearing in the comics
  , publishedDay :: Maybe Integer -- ^ Day the comics was published
  , photonovel :: Maybe Bool -- ^ Whether it's a photonovel
  , coverYear :: Maybe Integer -- ^ Cover publication year
  , title :: Text -- ^ Comics title
  , comicSeries :: Maybe [ComicSeriesBase] -- ^ Comic series this comics is included in
  , coverDay :: Maybe Integer -- ^ Cover publication day
  , references :: Maybe [Reference] -- ^ References
  , yearTo :: Maybe Integer -- ^ Ending year of comic story
  , staff :: Maybe [StaffBase] -- ^ Other staff involved in the comics
  , numberOfPages :: Maybe Integer -- ^ Number of pages
  , writers :: Maybe [StaffBase] -- ^ Writers involved in the comics
  , coverMonth :: Maybe Integer -- ^ Cover publication month
  , editors :: Maybe [StaffBase] -- ^ Editors involved in the comics
  }
  deriving (Eq, Show)

comicsFullSchema :: FC.Fleece schema => schema ComicsFull
comicsFullSchema =
  FC.object $
    FC.constructor ComicsFull
      #+ FC.optional "comicCollections" comicCollections (FC.list comicCollectionBaseSchema)
      #+ FC.optional "yearFrom" yearFrom FC.integer
      #+ FC.optional "stardateTo" stardateTo FC.float
      #+ FC.optional "adaptation" adaptation FC.boolean
      #+ FC.optional "publishers" publishers (FC.list companyBaseSchema)
      #+ FC.optional "publishedMonth" publishedMonth FC.integer
      #+ FC.optional "publishedYear" publishedYear FC.integer
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "stardateFrom" stardateFrom FC.float
      #+ FC.optional "artists" artists (FC.list staffBaseSchema)
      #+ FC.optional "characters" characters (FC.list characterBaseSchema)
      #+ FC.optional "publishedDay" publishedDay FC.integer
      #+ FC.optional "photonovel" photonovel FC.boolean
      #+ FC.optional "coverYear" coverYear FC.integer
      #+ FC.required "title" title FC.text
      #+ FC.optional "comicSeries" comicSeries (FC.list comicSeriesBaseSchema)
      #+ FC.optional "coverDay" coverDay FC.integer
      #+ FC.optional "references" references (FC.list referenceSchema)
      #+ FC.optional "yearTo" yearTo FC.integer
      #+ FC.optional "staff" staff (FC.list staffBaseSchema)
      #+ FC.optional "numberOfPages" numberOfPages FC.integer
      #+ FC.optional "writers" writers (FC.list staffBaseSchema)
      #+ FC.optional "coverMonth" coverMonth FC.integer
      #+ FC.optional "editors" editors (FC.list staffBaseSchema)