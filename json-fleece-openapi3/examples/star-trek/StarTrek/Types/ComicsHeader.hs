{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ComicsHeader
  ( ComicsHeader(..)
  , comicsHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.Types.ComicsHeader.Title as Title
import qualified StarTrek.Types.ComicsHeader.Uid as Uid

data ComicsHeader = ComicsHeader
  { title :: Title.Title -- ^ Comics title
  , uid :: Uid.Uid -- ^ Comics unique ID
  }
  deriving (Eq, Show)

comicsHeaderSchema :: FC.Fleece t => FC.Schema t ComicsHeader
comicsHeaderSchema =
  FC.object $
    FC.constructor ComicsHeader
      #+ FC.required "title" title Title.titleSchema
      #+ FC.required "uid" uid Uid.uidSchema