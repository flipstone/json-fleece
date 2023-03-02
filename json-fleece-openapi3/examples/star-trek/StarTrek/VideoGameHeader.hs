{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoGameHeader
  ( VideoGameHeader(..)
  , videoGameHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.VideoGameHeader.Title as Title
import qualified StarTrek.VideoGameHeader.Uid as Uid

data VideoGameHeader = VideoGameHeader
  { uid :: Uid.Uid -- ^ Video game unique ID
  , title :: Title.Title -- ^ Video game title
  }
  deriving (Eq, Show)

videoGameHeaderSchema :: FC.Fleece schema => schema VideoGameHeader
videoGameHeaderSchema =
  FC.object $
    FC.constructor VideoGameHeader
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.required "title" title Title.titleSchema