{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SoundtrackHeader
  ( SoundtrackHeader(..)
  , soundtrackHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.Types.SoundtrackHeader.Title as Title
import qualified StarTrek.Types.SoundtrackHeader.Uid as Uid

data SoundtrackHeader = SoundtrackHeader
  { title :: Title.Title -- ^ Soundtrack title
  , uid :: Uid.Uid -- ^ Soundtrack unique ID
  }
  deriving (Eq, Show)

soundtrackHeaderSchema :: FC.Fleece t => FC.Schema t SoundtrackHeader
soundtrackHeaderSchema =
  FC.object $
    FC.constructor SoundtrackHeader
      #+ FC.required "title" title Title.titleSchema
      #+ FC.required "uid" uid Uid.uidSchema