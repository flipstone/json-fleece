{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookHeader
  ( BookHeader(..)
  , bookHeaderSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Show)

data BookHeader = BookHeader
  { uid :: Text -- ^ Book unique ID
  , title :: Text -- ^ Book title
  }
  deriving (Eq, Show)

bookHeaderSchema :: FC.Fleece schema => schema BookHeader
bookHeaderSchema =
  FC.object $
    FC.constructor BookHeader
      #+ FC.required "uid" uid FC.text
      #+ FC.required "title" title FC.text