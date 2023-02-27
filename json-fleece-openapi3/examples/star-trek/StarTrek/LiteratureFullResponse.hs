{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.LiteratureFullResponse
  ( LiteratureFullResponse(..)
  , literatureFullResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.LiteratureFull (LiteratureFull, literatureFullSchema)

data LiteratureFullResponse = LiteratureFullResponse
  { literature :: Maybe LiteratureFull -- ^ Full literature, returned when queried using UID
  }
  deriving (Eq, Show)

literatureFullResponseSchema :: FC.Fleece schema => schema LiteratureFullResponse
literatureFullResponseSchema =
  FC.object $
    FC.constructor LiteratureFullResponse
      #+ FC.optional "literature" literature literatureFullSchema