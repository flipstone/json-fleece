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
      #+ FC.optionalField FC.OmitKey_DelegateNull "yearFrom" yearFrom FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "stardateTo" stardateTo FC.number
      #+ FC.required "audiobookAbridged" audiobookAbridged FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "audiobookPublishedDay" audiobookPublishedDay FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "productionNumber" productionNumber FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "publishedMonth" publishedMonth FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "publishedYear" publishedYear FC.integer
      #+ FC.required "uid" uid FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "stardateFrom" stardateFrom FC.number
      #+ FC.optionalField FC.OmitKey_DelegateNull "publishedDay" publishedDay FC.integer
      #+ FC.required "novel" novel FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "audiobookRunTime" audiobookRunTime FC.integer
      #+ FC.required "title" title FC.text
      #+ FC.required "referenceBook" referenceBook FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "audiobookPublishedMonth" audiobookPublishedMonth FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "yearTo" yearTo FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "audiobookPublishedYear" audiobookPublishedYear FC.integer
      #+ FC.required "biographyBook" biographyBook FC.boolean
      #+ FC.required "rolePlayingBook" rolePlayingBook FC.boolean
      #+ FC.required "novelization" novelization FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "numberOfPages" numberOfPages FC.integer
      #+ FC.required "eBook" eBook FC.boolean
      #+ FC.required "audiobook" audiobook FC.boolean