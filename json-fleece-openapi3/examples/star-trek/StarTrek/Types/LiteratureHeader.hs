{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.LiteratureHeader
  ( LiteratureHeader(..)
  , literatureHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.Types.LiteratureHeader.Title as Title
import qualified StarTrek.Types.LiteratureHeader.Uid as Uid

data LiteratureHeader = LiteratureHeader
  { uid :: Uid.Uid -- ^ Literature unique ID
  , title :: Title.Title -- ^ Literature title
  }
  deriving (Eq, Show)

literatureHeaderSchema :: FC.Fleece schema => schema LiteratureHeader
literatureHeaderSchema =
  FC.object $
    FC.constructor LiteratureHeader
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.required "title" title Title.titleSchema