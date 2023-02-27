{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TechnologyFullResponse
  ( TechnologyFullResponse(..)
  , technologyFullResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.TechnologyFull (TechnologyFull, technologyFullSchema)

data TechnologyFullResponse = TechnologyFullResponse
  { technology :: Maybe TechnologyFull -- ^ Full technology, returned when queried using UID
  }
  deriving (Eq, Show)

technologyFullResponseSchema :: FC.Fleece schema => schema TechnologyFullResponse
technologyFullResponseSchema =
  FC.object $
    FC.constructor TechnologyFullResponse
      #+ FC.optional "technology" technology technologyFullSchema