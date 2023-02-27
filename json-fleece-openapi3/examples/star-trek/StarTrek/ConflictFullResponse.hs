{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ConflictFullResponse
  ( ConflictFullResponse(..)
  , conflictFullResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ConflictFull (ConflictFull, conflictFullSchema)

data ConflictFullResponse = ConflictFullResponse
  { conflict :: Maybe ConflictFull -- ^ Base conflict, returned in search results
  }
  deriving (Eq, Show)

conflictFullResponseSchema :: FC.Fleece schema => schema ConflictFullResponse
conflictFullResponseSchema =
  FC.object $
    FC.constructor ConflictFullResponse
      #+ FC.optional "conflict" conflict conflictFullSchema