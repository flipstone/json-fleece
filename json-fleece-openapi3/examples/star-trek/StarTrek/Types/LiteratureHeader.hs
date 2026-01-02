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
  { title :: Title.Title -- ^ Literature title
  , uid :: Uid.Uid -- ^ Literature unique ID
  }
  deriving (Eq, Show)

literatureHeaderSchema :: FC.Fleece t => FC.Schema t LiteratureHeader
literatureHeaderSchema =
  FC.object $
    FC.constructor LiteratureHeader
      #+ FC.required "title" title Title.titleSchema
      #+ FC.required "uid" uid Uid.uidSchema