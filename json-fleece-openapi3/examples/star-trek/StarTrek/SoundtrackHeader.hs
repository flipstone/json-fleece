{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SoundtrackHeader
  ( SoundtrackHeader(..)
  , soundtrackHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.SoundtrackHeader.Title as Title
import qualified StarTrek.SoundtrackHeader.Uid as Uid

data SoundtrackHeader = SoundtrackHeader
  { uid :: Uid.Uid -- ^ Soundtrack unique ID
  , title :: Title.Title -- ^ Soundtrack title
  }
  deriving (Eq, Show)

soundtrackHeaderSchema :: FC.Fleece schema => schema SoundtrackHeader
soundtrackHeaderSchema =
  FC.object $
    FC.constructor SoundtrackHeader
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.required "title" title Title.titleSchema