{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookCollectionFull
  ( BookCollectionFull(..)
  , bookCollectionFullSchema
  ) where

import qualified Fleece.Core as FC
import Data.Scientific (Scientific)
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Integer, Maybe, Show)
import StarTrek.BookBase (BookBase, bookBaseSchema)
import StarTrek.BookSeriesBase (BookSeriesBase, bookSeriesBaseSchema)
import StarTrek.CharacterBase (CharacterBase, characterBaseSchema)
import StarTrek.CompanyBase (CompanyBase, companyBaseSchema)
import StarTrek.Reference (Reference, referenceSchema)
import StarTrek.StaffBase (StaffBase, staffBaseSchema)

data BookCollectionFull = BookCollectionFull
  { authors :: Maybe [StaffBase] -- ^ Authors of the book collection
  , yearFrom :: Maybe Integer -- ^ Starting year of book collection stories
  , stardateTo :: Maybe Scientific -- ^ Ending stardate of book collection stories
  , publishers :: Maybe [CompanyBase] -- ^ Book collection publishers
  , bookSeries :: Maybe [BookSeriesBase] -- ^ Book series this book collection is included in
  , publishedMonth :: Maybe Integer -- ^ Month the book collection was published
  , publishedYear :: Maybe Integer -- ^ Year the book collection was published
  , books :: Maybe [BookBase] -- ^ Books included in this book collection
  , uid :: Maybe Text -- ^ Book collection unique ID
  , stardateFrom :: Maybe Scientific -- ^ Starting stardate of book collection stories
  , artists :: Maybe [StaffBase] -- ^ Artists involved in the book collection
  , characters :: Maybe [CharacterBase] -- ^ Characters appearing in the book collection
  , publishedDay :: Maybe Integer -- ^ Day the book collection was published
  , title :: Maybe Text -- ^ Book collection title
  , references :: Maybe [Reference] -- ^ References
  , yearTo :: Maybe Integer -- ^ Ending year of book collection stories
  , numberOfPages :: Maybe Integer -- ^ Number of pages
  , editors :: Maybe [StaffBase] -- ^ Editors involved in the book collection
  }
  deriving (Eq, Show)

bookCollectionFullSchema :: FC.Fleece schema => schema BookCollectionFull
bookCollectionFullSchema =
  FC.object $
    FC.constructor BookCollectionFull
      #+ FC.optionalField FC.OmitKey_DelegateNull "authors" authors (FC.list staffBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "yearFrom" yearFrom FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "stardateTo" stardateTo FC.number
      #+ FC.optionalField FC.OmitKey_DelegateNull "publishers" publishers (FC.list companyBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "bookSeries" bookSeries (FC.list bookSeriesBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "publishedMonth" publishedMonth FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "publishedYear" publishedYear FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "books" books (FC.list bookBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "uid" uid FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "stardateFrom" stardateFrom FC.number
      #+ FC.optionalField FC.OmitKey_DelegateNull "artists" artists (FC.list staffBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "characters" characters (FC.list characterBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "publishedDay" publishedDay FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "title" title FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "references" references (FC.list referenceSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "yearTo" yearTo FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "numberOfPages" numberOfPages FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "editors" editors (FC.list staffBaseSchema)