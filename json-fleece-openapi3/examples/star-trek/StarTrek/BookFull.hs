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
      #+ FC.optional "audiobookPublishers" audiobookPublishers (FC.list companyBaseSchema)
      #+ FC.optional "authors" authors (FC.list staffBaseSchema)
      #+ FC.optional "yearFrom" yearFrom FC.integer
      #+ FC.optional "stardateTo" stardateTo FC.number
      #+ FC.optional "audiobookNarrators" audiobookNarrators (FC.list staffBaseSchema)
      #+ FC.required "audiobookAbridged" audiobookAbridged FC.boolean
      #+ FC.optional "publishers" publishers (FC.list companyBaseSchema)
      #+ FC.optional "bookSeries" bookSeries (FC.list bookSeriesBaseSchema)
      #+ FC.optional "audiobookPublishedDay" audiobookPublishedDay FC.integer
      #+ FC.optional "productionNumber" productionNumber FC.text
      #+ FC.optional "publishedMonth" publishedMonth FC.integer
      #+ FC.optional "publishedYear" publishedYear FC.integer
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "bookCollections" bookCollections (FC.list bookCollectionBaseSchema)
      #+ FC.optional "stardateFrom" stardateFrom FC.number
      #+ FC.optional "artists" artists (FC.list staffBaseSchema)
      #+ FC.optional "characters" characters (FC.list characterBaseSchema)
      #+ FC.optional "publishedDay" publishedDay FC.integer
      #+ FC.required "novel" novel FC.boolean
      #+ FC.optional "audiobookRunTime" audiobookRunTime FC.integer
      #+ FC.required "title" title FC.text
      #+ FC.required "referenceBook" referenceBook FC.boolean
      #+ FC.optional "references" references (FC.list referenceSchema)
      #+ FC.optional "audiobookPublishedMonth" audiobookPublishedMonth FC.integer
      #+ FC.optional "yearTo" yearTo FC.integer
      #+ FC.optional "audiobookReferences" audiobookReferences (FC.list referenceSchema)
      #+ FC.optional "audiobookPublishedYear" audiobookPublishedYear FC.integer
      #+ FC.required "biographyBook" biographyBook FC.boolean
      #+ FC.required "rolePlayingBook" rolePlayingBook FC.boolean
      #+ FC.required "novelization" novelization FC.boolean
      #+ FC.optional "numberOfPages" numberOfPages FC.integer
      #+ FC.required "eBook" eBook FC.boolean
      #+ FC.optional "editors" editors (FC.list staffBaseSchema)
      #+ FC.required "audiobook" audiobook FC.boolean