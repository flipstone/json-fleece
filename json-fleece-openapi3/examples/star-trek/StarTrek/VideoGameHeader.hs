{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoGameHeader
  ( VideoGameHeader(..)
  , videoGameHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import StarTrek.VideoGameHeader.Title (Title, titleSchema)
import StarTrek.VideoGameHeader.Uid (Uid, uidSchema)

data VideoGameHeader = VideoGameHeader
  { uid :: Uid -- ^ Video game unique ID
  , title :: Title -- ^ Video game title
  }
  deriving (Eq, Show)

videoGameHeaderSchema :: FC.Fleece schema => schema VideoGameHeader
videoGameHeaderSchema =
  FC.object $
    FC.constructor VideoGameHeader
      #+ FC.required "uid" uid uidSchema
      #+ FC.required "title" title titleSchema