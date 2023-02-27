{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.LiteratureHeader
  ( LiteratureHeader(..)
  , literatureHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import StarTrek.LiteratureHeader.Title (Title, titleSchema)
import StarTrek.LiteratureHeader.Uid (Uid, uidSchema)

data LiteratureHeader = LiteratureHeader
  { uid :: Uid -- ^ Literature unique ID
  , title :: Title -- ^ Literature title
  }
  deriving (Eq, Show)

literatureHeaderSchema :: FC.Fleece schema => schema LiteratureHeader
literatureHeaderSchema =
  FC.object $
    FC.constructor LiteratureHeader
      #+ FC.required "uid" uid uidSchema
      #+ FC.required "title" title titleSchema