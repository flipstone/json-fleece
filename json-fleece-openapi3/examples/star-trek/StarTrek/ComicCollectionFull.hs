{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicCollectionFull
  ( ComicCollectionFull(..)
  , comicCollectionFullSchema
  ) where

import Data.Text (Text)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Bool, Eq, Float, Integer, Maybe, Show)
import StarTrek.CharacterBase (CharacterBase, characterBaseSchema)
import StarTrek.ComicSeriesBase (ComicSeriesBase, comicSeriesBaseSchema)
import StarTrek.ComicsBase (ComicsBase, comicsBaseSchema)
import StarTrek.CompanyBase (CompanyBase, companyBaseSchema)
import StarTrek.Reference (Reference, referenceSchema)
import StarTrek.StaffBase (StaffBase, staffBaseSchema)

data ComicCollectionFull = ComicCollectionFull
  { yearFrom :: Maybe Integer -- ^ Starting year of comic collection stories
  , stardateTo :: Maybe Float -- ^ Ending stardate of comic collection stories
  , publishers :: Maybe [CompanyBase] -- ^ Comic collection publishers
  , publishedMonth :: Maybe Integer -- ^ Month the comic collection was published
  , publishedYear :: Maybe Integer -- ^ Year the comic collection was published
  , uid :: Text -- ^ Comic collection unique ID
  , stardateFrom :: Maybe Float -- ^ Starting stardate of comic collection stories
  , artists :: Maybe [StaffBase] -- ^ Artists involved in the comic collection
  , characters :: Maybe [CharacterBase] -- ^ Characters appearing in the comic collection
  , publishedDay :: Maybe Integer -- ^ Day the comic collection was published
  , photonovel :: Maybe Bool -- ^ Whether it's a photonovel collection
  , coverYear :: Maybe Integer -- ^ Cover publication year
  , title :: Text -- ^ Comic collection title
  , comicSeries :: Maybe [ComicSeriesBase] -- ^ Comic series this comic collection is included in
  , coverDay :: Maybe Integer -- ^ Cover publication day
  , references :: Maybe [Reference] -- ^ References
  , yearTo :: Maybe Integer -- ^ Ending year of comic collection stories
  , staff :: Maybe [StaffBase] -- ^ Other staff involved in the comic collection
  , comics :: Maybe [ComicsBase] -- ^ Comics included in this comic collection
  , numberOfPages :: Maybe Integer -- ^ Number of pages
  , writers :: Maybe [StaffBase] -- ^ Writers involved in the comic collection
  , coverMonth :: Maybe Integer -- ^ Cover publication month
  , editors :: Maybe [StaffBase] -- ^ Editors involved in the comic collection
  }
  deriving (Eq, Show)

comicCollectionFullSchema :: FC.Fleece schema => schema ComicCollectionFull
comicCollectionFullSchema =
  FC.object $
    FC.constructor ComicCollectionFull
      #+ FC.optional "yearFrom" yearFrom FC.integer
      #+ FC.optional "stardateTo" stardateTo FC.float
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
      #+ FC.optional "comics" comics (FC.list comicsBaseSchema)
      #+ FC.optional "numberOfPages" numberOfPages FC.integer
      #+ FC.optional "writers" writers (FC.list staffBaseSchema)
      #+ FC.optional "coverMonth" coverMonth FC.integer
      #+ FC.optional "editors" editors (FC.list staffBaseSchema)