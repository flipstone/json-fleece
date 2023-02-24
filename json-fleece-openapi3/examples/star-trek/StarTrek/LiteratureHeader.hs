{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.LiteratureHeader
  ( LiteratureHeader(..)
  , literatureHeaderSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Show)

data LiteratureHeader = LiteratureHeader
  { uid :: Text -- ^ Literature unique ID
  , title :: Text -- ^ Literature title
  }
  deriving (Eq, Show)

literatureHeaderSchema :: FC.Fleece schema => schema LiteratureHeader
literatureHeaderSchema =
  FC.object $
    FC.constructor LiteratureHeader
      #+ FC.required "uid" uid FC.text
      #+ FC.required "title" title FC.text