{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicsHeader
  ( ComicsHeader(..)
  , comicsHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import StarTrek.ComicsHeader.Title (Title, titleSchema)
import StarTrek.ComicsHeader.Uid (Uid, uidSchema)

data ComicsHeader = ComicsHeader
  { uid :: Uid -- ^ Comics unique ID
  , title :: Title -- ^ Comics title
  }
  deriving (Eq, Show)

comicsHeaderSchema :: FC.Fleece schema => schema ComicsHeader
comicsHeaderSchema =
  FC.object $
    FC.constructor ComicsHeader
      #+ FC.required "uid" uid uidSchema
      #+ FC.required "title" title titleSchema