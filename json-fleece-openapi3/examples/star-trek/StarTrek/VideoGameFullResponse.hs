{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoGameFullResponse
  ( VideoGameFullResponse(..)
  , videoGameFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.VideoGameFull (VideoGameFull, videoGameFullSchema)

data VideoGameFullResponse = VideoGameFullResponse
  { videoGame :: Maybe VideoGameFull -- ^ Full video game, returned when queried using UID
  }
  deriving (Eq, Show)

videoGameFullResponseSchema :: FC.Fleece schema => schema VideoGameFullResponse
videoGameFullResponseSchema =
  FC.object $
    FC.constructor VideoGameFullResponse
      #+ FC.optional "videoGame" videoGame videoGameFullSchema