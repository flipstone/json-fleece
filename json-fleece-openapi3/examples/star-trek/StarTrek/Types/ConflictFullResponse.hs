{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ConflictFullResponse
  ( ConflictFullResponse(..)
  , conflictFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.ConflictFull as ConflictFull

data ConflictFullResponse = ConflictFullResponse
  { conflict :: Maybe ConflictFull.ConflictFull -- ^ Base conflict, returned in search results
  }
  deriving (Eq, Show)

conflictFullResponseSchema :: FC.Fleece t => FC.Schema t ConflictFullResponse
conflictFullResponseSchema =
  FC.object $
    FC.constructor ConflictFullResponse
      #+ FC.optional "conflict" conflict ConflictFull.conflictFullSchema