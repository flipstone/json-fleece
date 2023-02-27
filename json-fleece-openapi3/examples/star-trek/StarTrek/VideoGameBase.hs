{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoGameBase
  ( VideoGameBase(..)
  , videoGameBaseSchema
  ) where

import Data.Text (Text)
import Data.Time (Day)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Float, Integer, Maybe, Show)

data VideoGameBase = VideoGameBase
  { yearFrom :: Maybe Integer -- ^ Starting year of video game story
  , stardateTo :: Maybe Float -- ^ Ending stardate of video game story
  , systemRequirements :: Maybe Text -- ^ System requirements
  , releaseDate :: Maybe Day -- ^ Release date
  , uid :: Text -- ^ Video game unique ID
  , stardateFrom :: Maybe Float -- ^ Starting stardate of video game story
  , title :: Text -- ^ Video game title
  , yearTo :: Maybe Integer -- ^ Ending year of video game story
  }
  deriving (Eq, Show)

videoGameBaseSchema :: FC.Fleece schema => schema VideoGameBase
videoGameBaseSchema =
  FC.object $
    FC.constructor VideoGameBase
      #+ FC.optional "yearFrom" yearFrom FC.integer
      #+ FC.optional "stardateTo" stardateTo FC.float
      #+ FC.optional "systemRequirements" systemRequirements FC.text
      #+ FC.optional "releaseDate" releaseDate FC.day
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "stardateFrom" stardateFrom FC.float
      #+ FC.required "title" title FC.text
      #+ FC.optional "yearTo" yearTo FC.integer