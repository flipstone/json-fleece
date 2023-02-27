{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoGameBase
  ( VideoGameBase(..)
  , videoGameBaseSchema
  ) where

import qualified Fleece.Core as FC
import Data.Scientific (Scientific)
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Integer, Maybe, Show)

data VideoGameBase = VideoGameBase
  { yearFrom :: Maybe Integer -- ^ Starting year of video game story
  , stardateTo :: Maybe Scientific -- ^ Ending stardate of video game story
  , systemRequirements :: Maybe Text -- ^ System requirements
  , releaseDate :: Maybe Text -- ^ Release date
  , uid :: Text -- ^ Video game unique ID
  , stardateFrom :: Maybe Scientific -- ^ Starting stardate of video game story
  , title :: Text -- ^ Video game title
  , yearTo :: Maybe Integer -- ^ Ending year of video game story
  }
  deriving (Eq, Show)

videoGameBaseSchema :: FC.Fleece schema => schema VideoGameBase
videoGameBaseSchema =
  FC.object $
    FC.constructor VideoGameBase
      #+ FC.optional "yearFrom" yearFrom FC.integer
      #+ FC.optional "stardateTo" stardateTo FC.number
      #+ FC.optional "systemRequirements" systemRequirements FC.text
      #+ FC.optional "releaseDate" releaseDate FC.text
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "stardateFrom" stardateFrom FC.number
      #+ FC.required "title" title FC.text
      #+ FC.optional "yearTo" yearTo FC.integer