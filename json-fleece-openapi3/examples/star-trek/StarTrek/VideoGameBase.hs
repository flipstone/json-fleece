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
      #+ FC.optionalField FC.OmitKey_DelegateNull "yearFrom" yearFrom FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "stardateTo" stardateTo FC.number
      #+ FC.optionalField FC.OmitKey_DelegateNull "systemRequirements" systemRequirements FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "releaseDate" releaseDate FC.text
      #+ FC.required "uid" uid FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "stardateFrom" stardateFrom FC.number
      #+ FC.required "title" title FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "yearTo" yearTo FC.integer