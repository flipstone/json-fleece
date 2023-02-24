{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicsHeader
  ( ComicsHeader(..)
  , comicsHeaderSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Show)

data ComicsHeader = ComicsHeader
  { uid :: Text -- ^ Comics unique ID
  , title :: Text -- ^ Comics title
  }
  deriving (Eq, Show)

comicsHeaderSchema :: FC.Fleece schema => schema ComicsHeader
comicsHeaderSchema =
  FC.object $
    FC.constructor ComicsHeader
      #+ FC.required "uid" uid FC.text
      #+ FC.required "title" title FC.text