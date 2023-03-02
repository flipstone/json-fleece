{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ConflictFullResponse
  ( ConflictFullResponse(..)
  , conflictFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.ConflictFull as ConflictFull

data ConflictFullResponse = ConflictFullResponse
  { conflict :: Maybe ConflictFull.ConflictFull -- ^ Base conflict, returned in search results
  }
  deriving (Eq, Show)

conflictFullResponseSchema :: FC.Fleece schema => schema ConflictFullResponse
conflictFullResponseSchema =
  FC.object $
    FC.constructor ConflictFullResponse
      #+ FC.optional "conflict" conflict ConflictFull.conflictFullSchema