{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MaterialFullResponse
  ( MaterialFullResponse(..)
  , materialFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.MaterialFull as MaterialFull

data MaterialFullResponse = MaterialFullResponse
  { material :: Maybe MaterialFull.MaterialFull -- ^ Full material, returned when queried using UID
  }
  deriving (Eq, Show)

materialFullResponseSchema :: FC.Fleece schema => schema MaterialFullResponse
materialFullResponseSchema =
  FC.object $
    FC.constructor MaterialFullResponse
      #+ FC.optional "material" material MaterialFull.materialFullSchema