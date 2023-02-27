{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookBase
  ( BookBase(..)
  , bookBaseSchema
  ) where

import qualified Fleece.Core as FC
import Data.Scientific (Scientific)
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Bool, Eq, Integer, Maybe, Show)

data BookBase = BookBase
  { anthology :: Bool -- ^ Whether it's an anthology
  , yearFrom :: Maybe Integer -- ^ Starting year of book story
  , stardateTo :: Maybe Scientific -- ^ Ending stardate of book story
  , audiobookAbridged :: Bool -- ^ If it's an audiobook, whether it's been abridged
  , audiobookPublishedDay :: Maybe Integer -- ^ Day the audiobook was published
  , productionNumber :: Maybe Text -- ^ Book's production number
  , publishedMonth :: Maybe Integer -- ^ Month the book was published
  , publishedYear :: Maybe Integer -- ^ Year the book was published
  , uid :: Text -- ^ Book unique ID
  , stardateFrom :: Maybe Scientific -- ^ Starting stardate of book story
  , publishedDay :: Maybe Integer -- ^ Day the book was published
  , novel :: Bool -- ^ Whether it's a novel
  , audiobookRunTime :: Maybe Integer -- ^ Audiobook run time, in minutes
  , title :: Text -- ^ Book title
  , referenceBook :: Bool -- ^ Whether it's a reference book
  , audiobookPublishedMonth :: Maybe Integer -- ^ Month the audiobook was published
  , yearTo :: Maybe Integer -- ^ Ending year of book story
  , audiobookPublishedYear :: Maybe Integer -- ^ Year the audiobook was published
  , biographyBook :: Bool -- ^ Whether it's a biography book
  , rolePlayingBook :: Bool -- ^ Whether it's a role playing book
  , novelization :: Bool -- ^ Whether it's a novelization
  , numberOfPages :: Maybe Integer -- ^ Number of pages
  , eBook :: Bool -- ^ Whether it's an eBook
  , audiobook :: Bool -- ^ Whether it's an audiobook, or has been release as an audiobook in addition to other form
  }
  deriving (Eq, Show)

bookBaseSchema :: FC.Fleece schema => schema BookBase
bookBaseSchema =
  FC.object $
    FC.constructor BookBase
      #+ FC.required "anthology" anthology FC.boolean
      #+ FC.optional "yearFrom" yearFrom FC.integer
      #+ FC.optional "stardateTo" stardateTo FC.number
      #+ FC.required "audiobookAbridged" audiobookAbridged FC.boolean
      #+ FC.optional "audiobookPublishedDay" audiobookPublishedDay FC.integer
      #+ FC.optional "productionNumber" productionNumber FC.text
      #+ FC.optional "publishedMonth" publishedMonth FC.integer
      #+ FC.optional "publishedYear" publishedYear FC.integer
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "stardateFrom" stardateFrom FC.number
      #+ FC.optional "publishedDay" publishedDay FC.integer
      #+ FC.required "novel" novel FC.boolean
      #+ FC.optional "audiobookRunTime" audiobookRunTime FC.integer
      #+ FC.required "title" title FC.text
      #+ FC.required "referenceBook" referenceBook FC.boolean
      #+ FC.optional "audiobookPublishedMonth" audiobookPublishedMonth FC.integer
      #+ FC.optional "yearTo" yearTo FC.integer
      #+ FC.optional "audiobookPublishedYear" audiobookPublishedYear FC.integer
      #+ FC.required "biographyBook" biographyBook FC.boolean
      #+ FC.required "rolePlayingBook" rolePlayingBook FC.boolean
      #+ FC.required "novelization" novelization FC.boolean
      #+ FC.optional "numberOfPages" numberOfPages FC.integer
      #+ FC.required "eBook" eBook FC.boolean
      #+ FC.required "audiobook" audiobook FC.boolean