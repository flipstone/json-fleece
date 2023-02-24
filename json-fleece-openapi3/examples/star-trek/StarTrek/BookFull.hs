{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookFull
  ( BookFull(..)
  , bookFullSchema
  ) where

import qualified Fleece.Core as FC
import Data.Scientific (Scientific)
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Bool, Eq, Integer, Maybe, Show)
import StarTrek.BookCollectionBase (BookCollectionBase, bookCollectionBaseSchema)
import StarTrek.BookSeriesBase (BookSeriesBase, bookSeriesBaseSchema)
import StarTrek.CharacterBase (CharacterBase, characterBaseSchema)
import StarTrek.CompanyBase (CompanyBase, companyBaseSchema)
import StarTrek.Reference (Reference, referenceSchema)
import StarTrek.StaffBase (StaffBase, staffBaseSchema)

data BookFull = BookFull
  { anthology :: Bool -- ^ Whether it's an anthology
  , audiobookPublishers :: Maybe [CompanyBase] -- ^ Audiobook publishers
  , authors :: Maybe [StaffBase] -- ^ Authors of the book
  , yearFrom :: Maybe Integer -- ^ Starting year of book story
  , stardateTo :: Maybe Scientific -- ^ Ending stardate of book story
  , audiobookNarrators :: Maybe [StaffBase] -- ^ Audiobook narrators
  , audiobookAbridged :: Bool -- ^ If it's an audiobook, whether it's been abridged
  , publishers :: Maybe [CompanyBase] -- ^ Book publishers
  , bookSeries :: Maybe [BookSeriesBase] -- ^ Book series this book is included in
  , audiobookPublishedDay :: Maybe Integer -- ^ Day the audiobook was published
  , productionNumber :: Maybe Text -- ^ Book production number
  , publishedMonth :: Maybe Integer -- ^ Month the book was published
  , publishedYear :: Maybe Integer -- ^ Year the book was published
  , uid :: Text -- ^ Book unique ID
  , bookCollections :: Maybe [BookCollectionBase] -- ^ Book collections this book is included in
  , stardateFrom :: Maybe Scientific -- ^ Starting stardate of book story
  , artists :: Maybe [StaffBase] -- ^ Artists involved in the book
  , characters :: Maybe [CharacterBase] -- ^ Characters appearing the book
  , publishedDay :: Maybe Integer -- ^ Day the book was published
  , novel :: Bool -- ^ Whether it's a novel
  , audiobookRunTime :: Maybe Integer -- ^ Audiobook run time, in minutes
  , title :: Text -- ^ Book title
  , referenceBook :: Bool -- ^ Whether it's a reference book
  , references :: Maybe [Reference] -- ^ References
  , audiobookPublishedMonth :: Maybe Integer -- ^ Month the audiobook was published
  , yearTo :: Maybe Integer -- ^ Ending year of book story
  , audiobookReferences :: Maybe [Reference] -- ^ Audiobook references
  , audiobookPublishedYear :: Maybe Integer -- ^ Year the audiobook was published
  , biographyBook :: Bool -- ^ Whether it's a biography book
  , rolePlayingBook :: Bool -- ^ Whether it's a role playing book
  , novelization :: Bool -- ^ Whether it's a novelization
  , numberOfPages :: Maybe Integer -- ^ Number of pages
  , eBook :: Bool -- ^ Whether it's an e-book
  , editors :: Maybe [StaffBase] -- ^ Editors involved in the book
  , audiobook :: Bool -- ^ Whether it's an audiobook, or has been release as an audiobook in addition to other form
  }
  deriving (Eq, Show)

bookFullSchema :: FC.Fleece schema => schema BookFull
bookFullSchema =
  FC.object $
    FC.constructor BookFull
      #+ FC.required "anthology" anthology FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "audiobookPublishers" audiobookPublishers (FC.list companyBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "authors" authors (FC.list staffBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "yearFrom" yearFrom FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "stardateTo" stardateTo FC.number
      #+ FC.optionalField FC.OmitKey_DelegateNull "audiobookNarrators" audiobookNarrators (FC.list staffBaseSchema)
      #+ FC.required "audiobookAbridged" audiobookAbridged FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "publishers" publishers (FC.list companyBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "bookSeries" bookSeries (FC.list bookSeriesBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "audiobookPublishedDay" audiobookPublishedDay FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "productionNumber" productionNumber FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "publishedMonth" publishedMonth FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "publishedYear" publishedYear FC.integer
      #+ FC.required "uid" uid FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "bookCollections" bookCollections (FC.list bookCollectionBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "stardateFrom" stardateFrom FC.number
      #+ FC.optionalField FC.OmitKey_DelegateNull "artists" artists (FC.list staffBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "characters" characters (FC.list characterBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "publishedDay" publishedDay FC.integer
      #+ FC.required "novel" novel FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "audiobookRunTime" audiobookRunTime FC.integer
      #+ FC.required "title" title FC.text
      #+ FC.required "referenceBook" referenceBook FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "references" references (FC.list referenceSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "audiobookPublishedMonth" audiobookPublishedMonth FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "yearTo" yearTo FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "audiobookReferences" audiobookReferences (FC.list referenceSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "audiobookPublishedYear" audiobookPublishedYear FC.integer
      #+ FC.required "biographyBook" biographyBook FC.boolean
      #+ FC.required "rolePlayingBook" rolePlayingBook FC.boolean
      #+ FC.required "novelization" novelization FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "numberOfPages" numberOfPages FC.integer
      #+ FC.required "eBook" eBook FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "editors" editors (FC.list staffBaseSchema)
      #+ FC.required "audiobook" audiobook FC.boolean