{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SoundtrackHeader
  ( SoundtrackHeader(..)
  , soundtrackHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import StarTrek.SoundtrackHeader.Title (Title, titleSchema)
import StarTrek.SoundtrackHeader.Uid (Uid, uidSchema)

data SoundtrackHeader = SoundtrackHeader
  { uid :: Uid -- ^ Soundtrack unique ID
  , title :: Title -- ^ Soundtrack title
  }
  deriving (Eq, Show)

soundtrackHeaderSchema :: FC.Fleece schema => schema SoundtrackHeader
soundtrackHeaderSchema =
  FC.object $
    FC.constructor SoundtrackHeader
      #+ FC.required "uid" uid uidSchema
      #+ FC.required "title" title titleSchema