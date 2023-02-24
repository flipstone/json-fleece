{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MovieHeader
  ( MovieHeader(..)
  , movieHeaderSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Show)

data MovieHeader = MovieHeader
  { uid :: Text -- ^ Movie unique ID
  , title :: Text -- ^ Movie title
  }
  deriving (Eq, Show)

movieHeaderSchema :: FC.Fleece schema => schema MovieHeader
movieHeaderSchema =
  FC.object $
    FC.constructor MovieHeader
      #+ FC.required "uid" uid FC.text
      #+ FC.required "title" title FC.text