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
  { uid :: Uid.Uid -- ^ Comics unique ID
  , title :: Title.Title -- ^ Comics title
  }
  deriving (Eq, Show)

comicsHeaderSchema :: FC.Fleece schema => schema ComicsHeader
comicsHeaderSchema =
  FC.object $
    FC.constructor ComicsHeader
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.required "title" title Title.titleSchema