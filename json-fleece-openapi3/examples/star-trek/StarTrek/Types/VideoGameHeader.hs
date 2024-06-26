{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.VideoGameHeader
  ( VideoGameHeader(..)
  , videoGameHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.Types.VideoGameHeader.Title as Title
import qualified StarTrek.Types.VideoGameHeader.Uid as Uid

data VideoGameHeader = VideoGameHeader
  { title :: Title.Title -- ^ Video game title
  , uid :: Uid.Uid -- ^ Video game unique ID
  }
  deriving (Eq, Show)

videoGameHeaderSchema :: FC.Fleece schema => schema VideoGameHeader
videoGameHeaderSchema =
  FC.object $
    FC.constructor VideoGameHeader
      #+ FC.required "title" title Title.titleSchema
      #+ FC.required "uid" uid Uid.uidSchema