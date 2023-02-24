{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicCollectionFull
  ( ComicCollectionFull(..)
  , comicCollectionFullSchema
  ) where

import qualified Fleece.Core as FC
import Data.Scientific (Scientific)
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Bool, Eq, Integer, Maybe, Show)
import StarTrek.CharacterBase (CharacterBase, characterBaseSchema)
import StarTrek.ComicSeriesBase (ComicSeriesBase, comicSeriesBaseSchema)
import StarTrek.ComicsBase (ComicsBase, comicsBaseSchema)
import StarTrek.CompanyBase (CompanyBase, companyBaseSchema)
import StarTrek.Reference (Reference, referenceSchema)
import StarTrek.StaffBase (StaffBase, staffBaseSchema)

data ComicCollectionFull = ComicCollectionFull
  { yearFrom :: Maybe Integer -- ^ Starting year of comic collection stories
  , stardateTo :: Maybe Scientific -- ^ Ending stardate of comic collection stories
  , publishers :: Maybe [CompanyBase] -- ^ Comic collection publishers
  , publishedMonth :: Maybe Integer -- ^ Month the comic collection was published
  , publishedYear :: Maybe Integer -- ^ Year the comic collection was published
  , uid :: Text -- ^ Comic collection unique ID
  , stardateFrom :: Maybe Scientific -- ^ Starting stardate of comic collection stories
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
      #+ FC.optionalField FC.OmitKey_DelegateNull "yearFrom" yearFrom FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "stardateTo" stardateTo FC.number
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
      #+ FC.optionalField FC.OmitKey_DelegateNull "comics" comics (FC.list comicsBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "numberOfPages" numberOfPages FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "writers" writers (FC.list staffBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "coverMonth" coverMonth FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "editors" editors (FC.list staffBaseSchema)