{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicsFull
  ( ComicsFull(..)
  , comicsFullSchema
  ) where

import qualified Fleece.Core as FC
import Data.Scientific (Scientific)
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Bool, Eq, Integer, Maybe, Show)
import StarTrek.CharacterBase (CharacterBase, characterBaseSchema)
import StarTrek.ComicCollectionBase (ComicCollectionBase, comicCollectionBaseSchema)
import StarTrek.ComicSeriesBase (ComicSeriesBase, comicSeriesBaseSchema)
import StarTrek.CompanyBase (CompanyBase, companyBaseSchema)
import StarTrek.Reference (Reference, referenceSchema)
import StarTrek.StaffBase (StaffBase, staffBaseSchema)

data ComicsFull = ComicsFull
  { comicCollections :: Maybe [ComicCollectionBase] -- ^ Comic collections this comics is included in
  , yearFrom :: Maybe Integer -- ^ Starting year of comic  story
  , stardateTo :: Maybe Scientific -- ^ Ending stardate of comic story
  , adaptation :: Maybe Bool -- ^ Whether it's an adaptation of an episode or a movie
  , publishers :: Maybe [CompanyBase] -- ^ Comics publishers
  , publishedMonth :: Maybe Integer -- ^ Month the comics was published
  , publishedYear :: Maybe Integer -- ^ Year the comics was published
  , uid :: Text -- ^ Comics unique ID
  , stardateFrom :: Maybe Scientific -- ^ Starting stardate of comic story
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
      #+ FC.optionalField FC.OmitKey_DelegateNull "comicCollections" comicCollections (FC.list comicCollectionBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "yearFrom" yearFrom FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "stardateTo" stardateTo FC.number
      #+ FC.optionalField FC.OmitKey_DelegateNull "adaptation" adaptation FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "publishers" publishers (FC.list companyBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "publishedMonth" publishedMonth FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "publishedYear" publishedYear FC.integer
      #+ FC.required "uid" uid FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "stardateFrom" stardateFrom FC.number
      #+ FC.optionalField FC.OmitKey_DelegateNull "artists" artists (FC.list staffBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "characters" characters (FC.list characterBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "publishedDay" publishedDay FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "photonovel" photonovel FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "coverYear" coverYear FC.integer
      #+ FC.required "title" title FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "comicSeries" comicSeries (FC.list comicSeriesBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "coverDay" coverDay FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "references" references (FC.list referenceSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "yearTo" yearTo FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "staff" staff (FC.list staffBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "numberOfPages" numberOfPages FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "writers" writers (FC.list staffBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "coverMonth" coverMonth FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "editors" editors (FC.list staffBaseSchema)