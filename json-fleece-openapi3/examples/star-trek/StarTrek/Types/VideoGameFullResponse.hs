{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.VideoGameFullResponse
  ( VideoGameFullResponse(..)
  , videoGameFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.VideoGameFull as VideoGameFull

data VideoGameFullResponse = VideoGameFullResponse
  { videoGame :: Maybe VideoGameFull.VideoGameFull -- ^ Full video game, returned when queried using UID
  }
  deriving (Eq, Show)

videoGameFullResponseSchema :: FC.Fleece schema => schema VideoGameFullResponse
videoGameFullResponseSchema =
  FC.object $
    FC.constructor VideoGameFullResponse
      #+ FC.optional "videoGame" videoGame VideoGameFull.videoGameFullSchema