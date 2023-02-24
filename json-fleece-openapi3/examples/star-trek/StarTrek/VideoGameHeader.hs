{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoGameHeader
  ( VideoGameHeader(..)
  , videoGameHeaderSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Show)

data VideoGameHeader = VideoGameHeader
  { uid :: Text -- ^ Video game unique ID
  , title :: Text -- ^ Video game title
  }
  deriving (Eq, Show)

videoGameHeaderSchema :: FC.Fleece schema => schema VideoGameHeader
videoGameHeaderSchema =
  FC.object $
    FC.constructor VideoGameHeader
      #+ FC.required "uid" uid FC.text
      #+ FC.required "title" title FC.text