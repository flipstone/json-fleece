{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ConflictBaseResponse
  ( ConflictBaseResponse(..)
  , conflictBaseResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ConflictBase (ConflictBase, conflictBaseSchema)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)

data ConflictBaseResponse = ConflictBaseResponse
  { sort :: Maybe ResponseSort -- ^ Response sort
  , conflicts :: Maybe [ConflictBase] -- ^ List of conflicts matching given criteria
  , page :: Maybe ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

conflictBaseResponseSchema :: FC.Fleece schema => schema ConflictBaseResponse
conflictBaseResponseSchema =
  FC.object $
    FC.constructor ConflictBaseResponse
      #+ FC.optional "sort" sort responseSortSchema
      #+ FC.optional "conflicts" conflicts (FC.list conflictBaseSchema)
      #+ FC.optional "page" page responsePageSchema