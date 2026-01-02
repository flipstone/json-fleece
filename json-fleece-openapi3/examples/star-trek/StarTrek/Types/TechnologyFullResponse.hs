{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TechnologyFullResponse
  ( TechnologyFullResponse(..)
  , technologyFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.TechnologyFull as TechnologyFull

data TechnologyFullResponse = TechnologyFullResponse
  { technology :: Maybe TechnologyFull.TechnologyFull -- ^ Full technology, returned when queried using UID
  }
  deriving (Eq, Show)

technologyFullResponseSchema :: FC.Fleece t => FC.Schema t TechnologyFullResponse
technologyFullResponseSchema =
  FC.object $
    FC.constructor TechnologyFullResponse
      #+ FC.optional "technology" technology TechnologyFull.technologyFullSchema